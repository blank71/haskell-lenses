module Lens.Put.Classic where

import Common
import Control.DeepSeq
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Type.Set (Proxy (..), (:++))
import Database.PostgreSQL.Simple.FromRow (FromRow (..))
import Delta (Delta, delta_union, negative, positive, (#+), (#-))
-- import FunDep (FunDep(..), Left, Right, TopologicalSort)

import qualified Delta
import FunDep
import GHC.TypeLits
import Label (AdjustOrder, IsSubset, Subtract)
import Lens (DeleteStrategy, Droppable, Fds, Joinable, Lens (..), Rt, Selectable, TableKey, Ts, deleteLeft, deleteRight, setDebugTime)
import Lens.Database.Base (Columns, LensDatabase (..), LensQuery, execute, get, query, queryEx)
import Lens.Database.Query (buildDelete, buildDeleteAll, buildInsert, buildUpdate, column_map, query_predicate)
import Lens.FunDep.Affected (Affected, ToDynamic, affected, toDPList)
import qualified Lens.Predicate.Base as P
import Lens.Predicate.Dynamic (DPhrase)
import qualified Lens.Predicate.Dynamic as DP
import Lens.Predicate.Hybrid (HPhrase (..))
import Lens.Record.Base (Env, InterCols, Project, ProjectEnv, VarsEnv)
import qualified Lens.Record.Base as R
import Lens.Record.Sorted (RecordsDelta, RecordsSet, Revisable, join, merge, project, revise_fd)
import qualified Lens.Record.Sorted as SR
import Tables (RecoverTables, recover_tables)
import qualified Value

put_classic_drop ::
  ( RecoverTables (Ts s),
    R.RecoverEnv (Rt s),
    LensQuery c,
    Droppable env (key :: [Symbol]) s snew
  ) =>
  c ->
  (Proxy key) ->
  (Proxy env) ->
  (Lens s) ->
  (Lens snew) ->
  RecordsSet (Rt snew) ->
  IO (RecordsSet (Rt s))
put_classic_drop c (Proxy :: Proxy key) (Proxy :: Proxy env) (l1 :: Lens s1) _ n =
  do
    old <- get c l1
    let mprime = join n envRows
    return $ revise_fd @(key --> P.Vars env) mprime old
  where
    envRows = Set.fromList [P.toRow @env]

put_classic_join ::
  forall c s1 s2 snew joincols.
  (LensQuery c, Joinable s1 s2 snew joincols) =>
  c ->
  (R.Row (Rt snew) -> DeleteStrategy) ->
  Lens s1 ->
  Lens s2 ->
  Lens snew ->
  RecordsSet (Rt snew) ->
  IO (RecordsSet (Rt s1), RecordsSet (Rt s2))
put_classic_join c delfn (l1 :: Lens s1) (l2 :: Lens s2) _ o =
  do
    m <- get c l1
    n <- get c l2
    let m0 = merge @(TopologicalSort (Fds s1)) m oleft
    let n0 = merge @(TopologicalSort (Fds s2)) n oright
    let l = join m0 n0 `Set.difference` o
    let ll = join @(Rt snew) l (project @joincols o)
    let la = l Set.\\ ll
    let m' = m0 `Set.difference` (project @(VarsEnv (Rt s1)) $ ll `Set.union` Set.filter (deleteLeft . delfn) la)
    let n' = n0 `Set.difference` (project @(VarsEnv (Rt s2)) $ Set.filter (deleteRight . delfn) la)
    return (m', n')
  where
    oleft = project @(VarsEnv (Rt s1)) o
    oright = project @(VarsEnv (Rt s2)) o

put_classic_select ::
  (LensQuery c, Selectable p s snew, RecoverTables (Ts s), R.RecoverEnv (Rt s)) =>
  c ->
  (HPhrase p) ->
  (Lens s) ->
  (Lens snew) ->
  RecordsSet (Rt snew) ->
  IO (RecordsSet (Rt s))
put_classic_select c (HPred p) (l :: Lens s) _ n =
  do
    m <- get c l
    let unsat = SR.filter (DP.not p) m
    let m0 = merge @(TopologicalSort (Fds s)) unsat n
    return m0

put_classic ::
  forall c s.
  (RecoverTables (Ts s), R.RecoverEnv (Rt s), LensQuery c, LensDatabase c) =>
  c ->
  (Lens s) ->
  RecordsSet (Rt s) ->
  IO ()
put_classic c (Prim :: Lens s) view =
  do
    qdelete <- buildDeleteAll c tbl
    action qdelete
    if Set.null view
      then return ()
      else do
        qinsert <- buildInsert c tbl $ Set.toList view
        action qinsert
  where
    tbl = head $ recover_tables @(Ts s) Proxy
    action = if False then Prelude.print else execute c
put_classic c (Debug l) view =
  do
    Prelude.print $ show view
    put_classic c l view
put_classic c dl@(DebugTime _ l) view =
  do
    SR.eval_strict view
    setDebugTime dl
    put_classic c l view
put_classic c l@(Drop key env l1) n =
  do
    res <- put_classic_drop c key env l1 l n
    put_classic c l1 res
put_classic c l@(Select p l1) n =
  do
    res <- put_classic_select c p l1 l n
    put_classic c l1 res
put_classic c l@(Join delfn l1 l2) o =
  do
    (m', n') <- put_classic_join c delfn l1 l2 l o
    put_classic c l1 m'
    put_classic c l2 n'

put_classic_wif ::
  forall c s.
  (RecoverTables (Ts s), R.RecoverEnv (Rt s), LensQuery c, LensDatabase c) =>
  c ->
  (Lens s) ->
  RecordsSet (Rt s) ->
  IO ()
put_classic_wif c (Prim :: Lens s) view =
  do
    qdelete <- buildDeleteAll c tbl
    action qdelete
    if Set.null view
      then return ()
      else do
        qinsert <- buildInsert c tbl $ Set.toList view
        action qinsert
  where
    tbl = head $ recover_tables @(Ts s) Proxy
    action = if True then Prelude.print else execute c
put_classic_wif c (Debug l) view =
  do
    Prelude.print $ show view
    put_classic c l view
put_classic_wif c dl@(DebugTime _ l) view =
  do
    let () = Set.toList view `deepseq` ()
    setDebugTime dl
    put_classic c l view
put_classic_wif c l@(Drop key env l1) n =
  do
    res <- put_classic_drop c key env l1 l n
    put_classic c l1 res
put_classic_wif c l@(Select p l1) n =
  do
    res <- put_classic_select c p l1 l n
    put_classic c l1 res
put_classic_wif c l@(Join delfn l1 l2) o =
  do
    (m', n') <- put_classic_join c delfn l1 l2 l o
    put_classic c l2 n'
    put_classic c l1 m'
