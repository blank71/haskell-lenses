{-
  Compilation is used to turn dynamic predicates into executable Haskell functions.
-}
module Lens.Predicate.Compile where

import qualified Data.List as List
import Data.Map.Strict ((!))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Type.Set (Proxy (..))
import GHC.TypeLits
import qualified Lens.Predicate.Base as P
import Lens.Predicate.Dynamic (BoxValue, DPhrase, Value, box)
import qualified Lens.Predicate.Dynamic as DP
import Lens.Record.Base (Row)
import qualified Lens.Record.Base as R

class LookupMap rt where
  lookupMap :: Map.Map String (Row rt -> Value)

instance LookupMap '[] where
  lookupMap = Map.empty

instance (KnownSymbol k, LookupMap rt, BoxValue t) => LookupMap ('(k, t) ': rt) where
  lookupMap = Map.insert (symbolVal (Proxy :: Proxy k)) thisF extMap
    where
      thisF :: Row ('(k, t) ': rt) -> Value
      thisF (R.Cons v _) = box v
      extMap = Map.map upd $ lookupMap @rt
      upd :: (Row rt -> Value) -> (Row ('(k, t) ': rt) -> Value)
      upd f (R.Cons _ r) = f r

logAnd :: Value -> Value -> Value
logAnd (DP.Bool True) (DP.Bool True) = DP.Bool True
logAnd (DP.Bool _) (DP.Bool _) = DP.Bool False
logAnd _ _ = error "Invalid arguments to logAnd"

logOr :: Value -> Value -> Value
logOr (DP.Bool False) (DP.Bool False) = DP.Bool False
logOr (DP.Bool _) (DP.Bool _) = DP.Bool True
logOr _ _ = error "Invalid arguments to logOr"

cmp :: Ordering -> Value -> Value -> Value
cmp o (DP.Bool b1) (DP.Bool b2) = DP.Bool (compare b1 b2 == o)
cmp o (DP.String s1) (DP.String s2) = DP.Bool (compare s1 s2 == o)
cmp o (DP.Int i1) (DP.Int i2) = DP.Bool (compare i1 i2 == o)
cmp _ _ _ = error "Invalid arguments to cmp"

plus :: Value -> Value -> Value
plus (DP.Int i1) (DP.Int i2) = DP.Int $ i1 + i2
plus _ _ = error "Invalid arguments to plus"

infixAppl :: P.Operator -> Value -> Value -> Value
infixAppl P.LessThan = cmp LT
infixAppl P.GreaterThan = cmp GT
infixAppl P.Equal = cmp EQ
infixAppl P.LogicalAnd = logAnd
infixAppl P.LogicalOr = logOr
infixAppl P.Plus = plus

unaryAppl :: P.UnaryOperator -> Value -> Value
unaryAppl P.UnaryMinus (DP.Int i) = DP.Int $ -i
unaryAppl P.Negate (DP.Bool b) = DP.Bool $ not b
unaryAppl _ _ = error "Invalid arguments to unaryAppl"

-- | Compile the dynamic predicate into a function taking a row and returning the computed value.
compile :: forall rt. LookupMap rt => DPhrase -> (Row rt -> Value)
compile (P.Constant v) = const v
compile (P.Var v) = lookupMap @rt ! v
compile (P.InfixAppl op p1 p2) = \r -> fop (f1 r) (f2 r)
  where
    fop = infixAppl op
    f1 = compile p1
    f2 = compile p2
compile (P.UnaryAppl op p) = fop . f
  where
    fop = unaryAppl op
    f = compile p
compile (P.In ids vs) = \r -> DP.Bool $ Set.member (fval r) valset
  where
    lookups = map (\i -> lookupMap @rt ! i) ids
    fval r = map (\v -> v r) lookups
    valset = Set.fromList vs
compile (P.Case p cases other) =
  \r ->
    let match = f r
     in case List.find (\(p1, _) -> p1 r == match) fcases of
          Just (_, p2) -> p2 r
          Nothing -> fother r
  where
    fdef _ = DP.Bool True
    f = maybe fdef compile p
    fcases = map (\(p1, p2) -> (compile @rt p1, compile @rt p2)) cases
    fother = compile other
compile _ = error "Unsupported predicate form"
