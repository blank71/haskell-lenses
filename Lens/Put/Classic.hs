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
import Lens.Record.Sorted (RecordsDelta, RecordsSet, Revisable, join, merge, project, reviseFd)
import qualified Lens.Record.Sorted as SR
import Tables (RecoverTables, recover_tables)
import qualified Value

putClassicDrop ::
  ( RecoverTables (Ts s),
    R.RecoverEnv (Rt s),
    LensQuery c,
    Droppable env (key :: [Symbol]) s snew
  ) =>
  c ->
  Proxy key ->
  Proxy env ->
  Lens s ->
  Lens snew ->
  RecordsSet (Rt snew) ->
  IO (RecordsSet (Rt s))
putClassicDrop c (Proxy :: Proxy key) (Proxy :: Proxy env) (l1 :: Lens s1) _ n =
  do
    old <- get c l1
    let mprime = join n envRows
    return $ reviseFd @(key --> P.Vars env) mprime old
  where
    envRows = Set.fromList [P.toRow @env]

putClassicJoin ::
  forall c s1 s2 snew joincols.
  (LensQuery c, Joinable s1 s2 snew joincols) =>
  c ->
  (R.Row (Rt snew) -> DeleteStrategy) ->
  Lens s1 ->
  Lens s2 ->
  Lens snew ->
  RecordsSet (Rt snew) ->
  IO (RecordsSet (Rt s1), RecordsSet (Rt s2))
putClassicJoin c delfn (l1 :: Lens s1) (l2 :: Lens s2) _ o =
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

putClassicSelect ::
  (LensQuery c, Selectable p s snew, RecoverTables (Ts s), R.RecoverEnv (Rt s)) =>
  c ->
  HPhrase p ->
  Lens s ->
  Lens snew ->
  RecordsSet (Rt snew) ->
  IO (RecordsSet (Rt s))
putClassicSelect c (HPred p) (l :: Lens s) _ n =
  do
    m <- get c l
    let unsat = SR.filter (DP.not p) m
    let m0 = merge @(TopologicalSort (Fds s)) unsat n
    return m0

putClassic ::
  forall c s.
  (RecoverTables (Ts s), R.RecoverEnv (Rt s), LensQuery c, LensDatabase c) =>
  c ->
  Lens s ->
  RecordsSet (Rt s) ->
  IO ()
putClassic c (Prim :: Lens s) view =
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
putClassic c (Debug l) view =
  do
    Prelude.print $ show view
    putClassic c l view
putClassic c dl@(DebugTime _ l) view =
  do
    SR.evalStrict view
    setDebugTime dl
    putClassic c l view
putClassic c l@(Drop key env l1) n =
  do
    res <- putClassicDrop c key env l1 l n
    putClassic c l1 res
putClassic c l@(Select p l1) n =
  do
    res <- putClassicSelect c p l1 l n
    putClassic c l1 res
putClassic c l@(Join delfn l1 l2) o =
  do
    (m', n') <- putClassicJoin c delfn l1 l2 l o
    putClassic c l1 m'
    putClassic c l2 n'

putClassicWif ::
  forall c s.
  (RecoverTables (Ts s), R.RecoverEnv (Rt s), LensQuery c, LensDatabase c) =>
  c ->
  Lens s ->
  RecordsSet (Rt s) ->
  IO ()
putClassicWif c (Prim :: Lens s) view =
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
putClassicWif c (Debug l) view =
  do
    Prelude.print $ show view
    putClassic c l view
putClassicWif c dl@(DebugTime _ l) view =
  do
    let () = Set.toList view `deepseq` ()
    setDebugTime dl
    putClassic c l view
putClassicWif c l@(Drop key env l1) n =
  do
    res <- putClassicDrop c key env l1 l n
    putClassic c l1 res
putClassicWif c l@(Select p l1) n =
  do
    res <- putClassicSelect c p l1 l n
    putClassic c l1 res
putClassicWif c l@(Join delfn l1 l2) o =
  do
    (m', n') <- putClassicJoin c delfn l1 l2 l o
    putClassic c l2 n'
    putClassic c l1 m'
