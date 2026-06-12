module Lens.Predicate.Dynamic where

import Common
import Control.DeepSeq
import Data.Text.Buildable (Buildable)
import Data.Text.Format
import Data.Text.Lazy.Builder
import Data.Type.Set
import GHC.TypeLits
import qualified Lens.Predicate.Base as P
import qualified Lens.Predicate.Precedence as QP

data Value where
  Bool :: Bool -> Value
  Int :: Int -> Value
  String :: String -> Value
  deriving (Eq, Ord)

instance NFData Value where
  rnf (Bool b) = rnf b
  rnf (Int i) = rnf i
  rnf (String s) = rnf s

class BoxValue t where
  box :: t -> Value

instance BoxValue Int where
  box = Int

instance BoxValue String where
  box = String

instance BoxValue Bool where
  box = Bool

-- data Predicate (r :: Env) where
--  Constant :: Value -> Predicate '[]
--  Var :: (KnownSymbol s, Recoverable t Types.Type) => Proxy '(s,t) -> Predicate '[ '(s, t)]
--  InfixAppl :: Operator -> Predicate r1 -> Predicate r2 -> Predicate ()

type Phrase = P.Phrase String Value

type DPhrase = Phrase

instance Recoverable b Bool => Recoverable ('P.Bool b) Value where
  recover Proxy = Bool (recover (Proxy :: Proxy b))

instance KnownNat i => Recoverable ('P.Int i) Value where
  recover Proxy = Int (fromIntegral $ natVal (Proxy :: Proxy i))

instance KnownSymbol s => Recoverable ('P.String s) Value where
  recover Proxy = String (symbolVal (Proxy :: Proxy s))

-- Phrase

instance Recoverable v Value => Recoverable ('P.Constant v) Phrase where
  recover Proxy = P.Constant (recover (Proxy :: Proxy v))

instance KnownSymbol v => Recoverable ('P.Var v) Phrase where
  recover Proxy = P.Var (symbolVal (Proxy :: Proxy v))

instance (Recoverable p1 Phrase, Recoverable p2 Phrase, Recoverable op P.Operator) => Recoverable ('P.InfixAppl op p1 p2) Phrase where
  recover Proxy = P.InfixAppl (recover @op Proxy) (recover @p1 Proxy) (recover @p2 Proxy)

instance (Recoverable p Phrase, Recoverable op P.UnaryOperator) => Recoverable ('P.UnaryAppl op p) Phrase where
  recover Proxy = P.UnaryAppl (recover @op Proxy) (recover @p Proxy)

instance
  (Recoverable cond (Maybe Phrase), Recoverable cases [(Phrase, Phrase)], Recoverable pother Phrase) =>
  Recoverable ('P.Case cond cases pother) Phrase
  where
  recover Proxy = P.Case (recover @cond @(Maybe Phrase) Proxy) (recover @cases @[(Phrase, Phrase)] Proxy) (recover @pother @Phrase Proxy)

simplify :: Phrase -> Phrase
simplify (P.InfixAppl P.LogicalAnd (P.Constant (Bool True)) p2) = p2
simplify (P.InfixAppl P.LogicalAnd (P.Constant (Bool False)) _) = P.Constant (Bool False)
simplify (P.InfixAppl P.LogicalAnd p1 (P.Constant (Bool True))) = p1
simplify (P.InfixAppl P.LogicalAnd _ (P.Constant (Bool False))) = P.Constant (Bool False)
simplify p = p

conjunction :: [P.Phrase id Value] -> P.Phrase id Value
conjunction (P.Constant (Bool True) : y : xs) = conjunction $ y : xs
conjunction (y : P.Constant (Bool True) : xs) = conjunction $ y : xs
conjunction (x : y : xs) = P.InfixAppl P.LogicalAnd x $ conjunction $ y : xs
conjunction [x] = x
conjunction [] = P.Constant (Bool True)

disjunction :: [P.Phrase id Value] -> P.Phrase id Value
disjunction (P.Constant (Bool False) : y : xs) = disjunction $ y : xs
disjunction (x : P.Constant (Bool False) : xs) = disjunction $ x : xs
disjunction (x : y : xs) = P.InfixAppl P.LogicalOr x $ disjunction $ y : xs
disjunction [x] = x
disjunction [] = P.Constant (Bool True)

not :: P.Phrase id Value -> P.Phrase id Value
not = P.UnaryAppl P.Negate

printValue :: Value -> IO Builder
printValue (Bool False) = return $ build "false" ()
printValue (Bool True) = return $ build "true" ()
printValue (Int i) = return $ build "{}" (Only i)
printValue (String s) = return $ build "{}" (Only s)

printOp :: P.Operator -> String
printOp P.Plus = "+"
printOp P.LogicalAnd = "AND"
printOp P.LogicalOr = "OR"
printOp P.Equal = "="
printOp P.LessThan = "<"
printOp P.GreaterThan = ">"

printUnaryOp :: P.UnaryOperator -> String
printUnaryOp P.Negate = "NOT"
printUnaryOp P.UnaryMinus = "-"

printQueryEq :: Phrase -> QP.Op -> QP.Op -> IO Builder
printQueryEq p pr npr
  | npr < pr = build "({})" . Only <$> printQuery p npr
  | otherwise = printQuery p npr

printQueryGr :: Phrase -> QP.Op -> QP.Op -> IO Builder
printQueryGr p pr npr
  | npr > pr = printQuery p npr
  | otherwise = build "({})" . Only <$> printQuery p npr

buildSep :: (Buildable sep, Buildable a) => sep -> [a] -> Builder
buildSep _ [] = build "" ()
buildSep _ [x] = build "{}" (Only x)
buildSep sep (x : xs) = build "{}{}{}" (x, sep, buildSep sep xs)

buildSepStr :: Buildable a => String -> [a] -> Builder
buildSepStr = buildSep

printQuery :: Phrase -> QP.Op -> IO Builder
printQuery (P.Constant val) _ = printValue val
printQuery (P.Var v) _ = return $ build "{}" v
printQuery (P.InfixAppl op a b) pr =
  let npr = QP.ofOp op
   in do
        left <- printQueryEq a pr npr
        right <- printQueryEq b pr npr
        return $ build "{} {} {}" (left, printOp op, right)
printQuery (P.UnaryAppl op a) pr =
  let npr = QP.ofUnaryOp op
   in do
        arg <- printQueryGr a pr npr
        return $ build "{} {}" (printUnaryOp op, arg)
printQuery (P.In _ []) _ =
  return $ build "FALSE" ()
printQuery (P.In cs vals) _ =
  do
    vals <- mapM build_vals vals
    return $ build "({}) IN ({})" (buildSepStr ", " cs, buildSepStr ", " vals)
  where
    build_vals vs =
      do
        vals <- mapM printValue vs
        return $ build "({})" $ Only $ buildSepStr ", " vals
printQuery (P.Case inp cases other) _ =
  do
    inp <- build_inp inp
    cases <- mapM build_case cases
    other <- printQuery other QP.first
    return $ build "CASE {}{} ELSE {} END" (inp, buildSepStr " " cases, other)
  where
    build_inp Nothing = return $ build "" ()
    build_inp (Just x) = build "({}) " . Only <$> printQuery x QP.first
    build_case (key, val) =
      do
        cond <- printQuery key QP.first
        act <- printQuery val QP.first
        return $ build "WHEN {} THEN {}" (cond, act)
printQuery _ _ = error "Unsupported query"

print :: Phrase -> IO Builder
print p = printQuery p QP.first
