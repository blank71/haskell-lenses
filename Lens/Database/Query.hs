module Lens.Database.Query where

import Common
import Control.Monad.State.Lazy
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Set as Set
import Data.Text.Buildable (Buildable)
import Data.Text.Format
import Data.Text.Lazy.Builder
import Data.Type.Set
import Lens (Lens (..), Rt, Ts)
import Lens.Database.Base (Columns, LensDatabase (..), LensQueryable)
import Lens.FunDep.Affected (ToDynamic, toDPList, toDynamic)
import Lens.Helpers.Format (buildSepComma, buildSepSpace)
import qualified Lens.Predicate.Base as P
import qualified Lens.Predicate.Dynamic as DP
import Lens.Predicate.Hybrid (HPhrase (..))
import qualified Lens.Predicate.Precedence as QP
import Lens.Record.Base (RecoverEnv, Row, VarsEnv, recover_env)
import Lens.Record.Sorted (RecordsSet)
import qualified Lens.Types as T
import Tables (RecoverTables, recover_tables)

type ColumnsOpt = Map.Map String (String, String)

class ColumnMap a where
  column_map :: a -> Columns

instance (RecoverTables (Ts s), RecoverEnv (Rt s)) => ColumnMap (Lens s) where
  column_map Prim = Map.fromList $ map f env
    where
      env = recover_env @(Rt s) Proxy
      f (col, typ) = (col, ([table_name], typ))
      table_name = head $ recover_tables @(Ts s) Proxy
  column_map (Debug l) = column_map l
  column_map (DebugTime _ l) = column_map l
  column_map (Select _ l) = column_map l
  column_map (Drop Proxy Proxy l) = column_map l
  column_map (Join _ l1 l2) = Map.unionWith f (column_map l1) (column_map l2)
    where
      f (t1, typ) (t2, _) = (t1 ++ t2, typ)

class QueryPredicate a where
  query_predicate :: a -> DP.Phrase

instance QueryPredicate (Lens s) where
  query_predicate Prim = P.Constant (DP.Bool True)
  query_predicate (Debug l) = query_predicate l
  query_predicate (DebugTime _ l) = query_predicate l
  query_predicate (Drop Proxy Proxy l) = query_predicate l
  query_predicate (Select (HPred pr) l) = DP.simplify $ P.InfixAppl P.LogicalAnd pr (query_predicate l)
  query_predicate (Join _ l1 l2) = DP.simplify $ P.InfixAppl P.LogicalAnd (query_predicate l1) (query_predicate l2)

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

eqPriority :: QP.Op -> QP.Op -> Builder -> Builder
eqPriority pr npr bld
  | npr < pr = build "({})" $ Only bld
  | otherwise = bld

grPriority :: QP.Op -> QP.Op -> Builder -> Builder
grPriority pr npr bld
  | npr > pr = bld
  | otherwise = build "({})" $ Only bld

printValue :: LensDatabase db => db -> DP.Value -> IO Builder
printValue db (DP.Bool False) = return $ build "FALSE" ()
printValue db (DP.Bool True) = return $ build "TRUE" ()
printValue db (DP.Int i) = return $ build "{}" (Only i)
printValue db (DP.String s) = escapeStr db s

printCol :: LensDatabase db => db -> String -> String -> IO Builder
printCol db tbl col =
  do
    etbl <- escapeId db tbl
    ecol <- escapeId db col
    return $ if tbl == "" then ecol else build "{}.{}" (etbl, ecol)

printColT :: LensDatabase db => db -> String -> ([String], T.Type) -> IO Builder
printColT db v (table, _) =
  printCol db (head table) v

printQuery :: LensDatabase db => db -> ColumnsOpt -> DP.Phrase -> QP.Op -> IO Builder
printQuery db _ (P.Constant val) _ = printValue db val
printQuery db cols (P.Var v) _ = printCol db tbl col
  where
    (col, tbl) = fromJust $ Map.lookup v cols
printQuery db cols (P.InfixAppl op a b) pr =
  let npr = QP.ofOp op
   in do
        left <- printQuery db cols a npr
        right <- printQuery db cols b npr
        return $ eqPriority pr npr $ build "{} {} {}" (left, printOp op, right)
printQuery db cols (P.UnaryAppl op a) pr =
  let npr = QP.ofUnaryOp op
   in do
        arg <- printQuery db cols a npr
        return $ grPriority pr npr $ build "{} {}" (printUnaryOp op, arg)
printQuery db _ (P.In _ []) _ =
  return $ build "FALSE" ()
printQuery db cols (P.In cs vals) pr =
  do
    vals <- mapM build_vals vals
    pcs <- mapM (\v -> printQuery db cols (P.Var v) pr) cs
    return $ build "({}) IN ({})" (buildSepComma pcs, buildSepComma vals)
  where
    build_vals vs =
      do
        vals <- mapM (printValue db) vs
        return $ build "({})" $ Only $ buildSepComma vals
printQuery db cols (P.Case inp cases other) _ =
  do
    inp <- build_inp inp
    cases <- mapM build_case cases
    other <- printQuery db cols other QP.first
    return $ build "CASE {}{} ELSE {} END" (inp, buildSepSpace cases, other)
  where
    build_inp Nothing = return $ build "" ()
    build_inp (Just x) = build "({}) " . Only <$> printQuery db cols x QP.first
    build_case (key, val) =
      do
        cond <- printQuery db cols key QP.first
        act <- printQuery db cols val QP.first
        return $ build "WHEN {} THEN {}" (cond, act)
printQuery _ _ _ _ =
  error "Impossible: Case with non-query predicate"

colsOpt :: Columns -> State (Int, [[String]]) ColumnsOpt
colsOpt cols = do
  es <- mapM f $ Map.toList cols
  return (Map.fromList $ concat es)
  where
    f (k, (tbls, _)) = entries k tbls
    fresh = do
      (id, cols) <- get
      put (id + 1, cols)
      return $ "__" ++ show id
    add_eqs cs = do
      (id, cols) <- get
      put (id, cs : cols)
      return ()
    fresh_entries col tbl = do
      id <- fresh
      return $ entry id col tbl
    entry k col tbl = (k, (col, tbl))
    entries k tbls = do
      others <- mapM (fresh_entries k) $ tail tbls
      add_eqs $ k : map fst others
      return $ entry k k (head tbls) : others

buildQueryEx ::
  forall r db.
  (LensDatabase db) =>
  db ->
  [String] ->
  [String] ->
  Columns ->
  DP.Phrase ->
  IO Builder
buildQueryEx db tbls cols cols_map p =
  do
    sel <- cols_bld
    from <- tbls_bld
    wher <- pred_bld
    return $ build "SELECT {} FROM {} WHERE {}" (sel, from, wher)
  where
    (cols', (_, grps)) = runState (colsOpt cols_map) (1, [])
    build_group (x : y : xs) = P.InfixAppl P.Equal (P.Var x) (P.Var y) : build_group (y : xs)
    build_group _ = []
    build_groups = DP.conjunction $ map (DP.conjunction . build_group) grps
    cols_bld = buildSepComma <$> mapM (\k -> printColT db k $ fromJust $ Map.lookup k cols_map) cols
    pred_bld = printQuery db cols' (DP.conjunction [build_groups, p]) QP.first
    tbls_bld = buildSepComma <$> mapM (fmap (build "{}" . Only) . escapeId db) tbls

buildQuery ::
  LensQueryable s =>
  LensDatabase db =>
  db ->
  Lens s ->
  IO Builder
buildQuery db (l :: Lens s) = buildQueryEx db tbls cols cols_map p
  where
    p = query_predicate l
    cols = map fst $ recover_env @(Rt s) Proxy
    cols_map = column_map l
    tbls = recover_tables @(Ts s) Proxy

buildInsertEx ::
  forall db.
  (LensDatabase db) =>
  db ->
  String ->
  [String] ->
  [[DP.Value]] ->
  IO Builder
buildInsertEx db tbl cols vals =
  do
    etbl <- escapeId db tbl
    colstr <- buildSepComma <$> mapM (escapeId db) cols
    valstr <- buildSepComma <$> mapM build_record vals
    return $ build "INSERT INTO {} ({}) VALUES {}" (etbl, colstr, valstr)
  where
    build_record rs = build "({})" . Only . buildSepComma <$> mapM (printValue db) rs

buildInsert ::
  forall db rt.
  (ToDynamic rt, Recoverable (VarsEnv rt) [String], LensDatabase db) =>
  db ->
  String ->
  [Row rt] ->
  IO Builder
buildInsert db tbl rs = buildInsertEx db tbl cols vals
  where
    vals = toDPList rs
    cols = recover @(VarsEnv rt) @[String] Proxy

buildDeleteEx :: forall db. (LensDatabase db) => db -> String -> [(String, DP.Value)] -> IO Builder
buildDeleteEx db tbl match =
  do
    etbl <- escapeId db tbl
    wher <- printQuery db colsOpt pred QP.first
    return $ build "DELETE FROM {} WHERE {}" (etbl, wher)
  where
    colsOpt = Map.fromList $ map (\(k, _) -> (k, (k, ""))) match
    pred = DP.conjunction $ map (\(k, v) -> P.InfixAppl P.Equal (P.Var k) (P.Constant v)) match

buildDelete ::
  forall db rt.
  (ToDynamic rt, Recoverable (VarsEnv rt) [String], LensDatabase db) =>
  db ->
  String ->
  Row rt ->
  IO Builder
buildDelete db tbl match = buildDeleteEx db tbl matchex
  where
    cols = recover @(VarsEnv rt) Proxy
    vals = toDynamic match
    matchex = zip cols vals

buildDeleteAll :: forall db. (LensDatabase db) => db -> String -> IO Builder
buildDeleteAll db tbl =
  do
    etbl <- escapeId db tbl
    return $ build "DELETE FROM {} WHERE TRUE" (Only etbl)

buildUpdateEx ::
  forall db.
  LensDatabase db =>
  db ->
  String ->
  [(String, DP.Value)] ->
  [(String, DP.Value)] ->
  IO Builder
buildUpdateEx db tbl match update =
  do
    etbl <- escapeId db tbl
    eset <- buildSepComma <$> mapM fset update
    ewher <- printQuery db colsOpt pred QP.first
    return $ build "UPDATE {} SET {} WHERE {}" (etbl, eset, ewher)
  where
    colsOpt = Map.fromList $ map (\(k, _) -> (k, (k, ""))) match
    pred = DP.conjunction $ map (\(k, v) -> P.InfixAppl P.Equal (P.Var k) (P.Constant v)) match
    fset (k, v) =
      do
        ek <- printCol db "" k
        ev <- printValue db v
        return $ build "{} = {}" (ek, ev)

buildUpdate ::
  forall db rtm rtu.
  ( Recoverable (VarsEnv rtm) [String],
    ToDynamic rtm,
    Recoverable (VarsEnv rtu) [String],
    ToDynamic rtu,
    LensDatabase db
  ) =>
  db ->
  String ->
  Row rtm ->
  Row rtu ->
  IO Builder
buildUpdate db tbl match update =
  buildUpdateEx db tbl matchex updex
  where
    colsm = recover @(VarsEnv rtm) Proxy
    colsu = recover @(VarsEnv rtu) Proxy
    dmatch = toDynamic match
    dupd = toDynamic update
    matchex = zip colsm dmatch
    updex = zip colsu dupd

combineQueries :: Foldable t => t Builder -> Builder
combineQueries = foldl1 (curry (build "{}; {}"))

runMultiple :: Foldable t => (Builder -> IO ()) -> t Builder -> IO ()
runMultiple action qs =
  if null qs
    then return ()
    else action $ combineQueries qs
