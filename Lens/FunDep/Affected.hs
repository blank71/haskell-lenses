module Lens.FunDep.Affected where

import Common
import qualified Data.Set as Set
import Data.Type.Set (Proxy (..))
import FunDep (FunDep (..), Left)
import qualified Lens.Predicate.Base as P
import Lens.Predicate.Dynamic (BoxValue, DPhrase, box)
import qualified Lens.Predicate.Dynamic as DP
import Lens.Record.Base (Env)
import qualified Lens.Record.Base as R
import Lens.Record.Sorted (RecordsSet, project)
import qualified Value as V

class ToDynamic rt where
  toDynamic :: R.Row rt -> [DP.Value]

instance ToDynamic '[] where
  toDynamic _ = []

instance (BoxValue t, ToDynamic xs) => ToDynamic ('(k, t) ': xs) where
  toDynamic (R.Cons v rw) = box v : toDynamic rw

toDPList :: ToDynamic rt => [R.Row rt] -> [[DP.Value]]
toDPList = map toDynamic

class Affected (fds :: [FunDep]) (rt :: Env) where
  affected :: RecordsSet rt -> DPhrase

instance Affected '[] rt where
  affected _ = P.Constant (DP.Bool False)

instance
  (Recoverable (Left fd) [String], R.Project (Left fd) rt, Affected fds rt, ToDynamic (R.ProjectEnv (Left fd) rt)) =>
  Affected (fd ': fds) rt
  where
  affected rt =
    DP.disjunction
      [ P.In (recover @(Left fd) Proxy) (toDPList $ Set.toList $ project @(Left fd) rt),
        affected @fds rt
      ]
