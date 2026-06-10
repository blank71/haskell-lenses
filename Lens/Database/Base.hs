module Lens.Database.Base where

import qualified Data.Map.Strict as Map
import Data.Set (Set, fromList)
import Data.Text.Lazy.Builder (Builder)
import Data.Type.Set (Proxy (..))
import Database.PostgreSQL.Simple.FromRow (FromRow (..))
import Lens (Lens, Rt, Ts)
import qualified Lens.Predicate.Dynamic as DP
import Lens.Record.Base (Env, RecoverEnv, Row)
import qualified Lens.Types as T
import Tables (RecoverTables)

type Tables = [String]

type Columns = Map.Map String ([String], T.Type)

type LensQueryable s = (RecoverTables (Ts s), RecoverEnv (Rt s))

class LensDatabase c where
  escapeId :: c -> String -> IO Builder
  escapeStr :: c -> String -> IO Builder

class LensQuery c where
  query ::
    forall s rt.
    (LensQueryable s, rt ~ Rt s, FromRow (Row rt)) =>
    c ->
    Lens s ->
    IO [Row rt]
  queryEx :: (FromRow (Row rt), RecoverEnv rt) => Proxy rt -> c -> Tables -> Columns -> DP.Phrase -> IO [Row rt]
  execute :: c -> Builder -> IO ()

queryEx' ::
  forall c rt.
  (RecoverEnv rt, LensQuery c, FromRow (Row rt)) =>
  c ->
  Tables ->
  Columns ->
  DP.Phrase ->
  IO [Row rt]
queryEx' = queryEx @c @rt Proxy

type LensGet s c = (LensQueryable s, FromRow (Row (Rt s)), LensQuery c)

get :: forall s c. LensGet s c => c -> Lens s -> IO (Set (Row (Rt s)))
get c l = do
  res <- query c l
  return $ fromList res
