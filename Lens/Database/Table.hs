{-
  A module for manipulating database table definitions.
-}
module Lens.Database.Table where

import Common
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Data.Text.Buildable (Buildable (..))
import Data.Text.Format (Only (..))
import qualified Data.Text.Format as F (build)
import Data.Text.Lazy.Builder (Builder)
import Data.Type.Set (Proxy (..))
import Lens (Fds, Lens (..), Rt, Ts)
import Lens.Database.Base (LensDatabase (..), LensQuery (..))
import Lens.Helpers.Format (buildSepComma)
import Lens.Record.Base (RecoverEnv (..))
import Lens.Types (Type (..))
import Tables (recover_tables)

data Column = Column
  { colName :: String,
    colTyp :: Type
  }
  deriving (Show)

data Table = Table
  { tblName :: String,
    tblColumns :: [Column],
    tblKey :: Maybe [String]
  }
  deriving (Show)

dbType :: Type -> String
dbType Int = "INT"
dbType String = "TEXT"
dbType Bool = "BOOL"

buildCol :: LensDatabase db => db -> Column -> IO Builder
buildCol db col =
  do
    name <- escapeId db $ colName col
    return $ F.build "{} {}" (name, dbType $ colTyp col)

buildCreateTblIfne :: LensDatabase db => db -> Table -> IO Builder
buildCreateTblIfne db tbl =
  do
    name <- escapeId db $ tblName tbl
    cols <- mapM (buildCol db) $ tblColumns tbl
    let pkcols = tblKey tbl
    keyOpt <- pk pkcols
    return $
      F.build
        "CREATE TABLE IF NOT EXISTS {} ({}{})"
        (name, buildSepComma cols, keyOpt)
  where
    pk (Just cols) =
      do
        cols <- mapM (escapeId db) cols
        return $ F.build ", PRIMARY KEY ({})" (Only $ buildSepComma cols)
    pk Nothing = return $ F.build "" ()

addForeignKey db fname tbl col ftbl fkey =
  do
    fnameId <- escapeId db fname
    tblId <- escapeId db tbl
    colId <- escapeId db col
    ftblId <- escapeId db ftbl
    fkeyId <- escapeId db fkey
    let bld = F.build "ALTER TABLE {} ADD CONSTRAINT {} FOREIGN KEY ({}) REFERENCES {}({})" (tblId, fname, colId, ftbl, fkeyId)
    execute db bld

-- | Create a database index called iname for table tbl and column col.
createIndex db iname tbl col =
  do
    inameId <- escapeId db iname
    tblId <- escapeId db tbl
    colId <- escapeId db col
    let bld =
          F.build
            "CREATE INDEX IF NOT EXISTS {} ON {} ({})"
            (inameId, tblId, colId)
    execute db bld

-- | Create the database table if it does not exist.
setup :: (LensQuery db, LensDatabase db) => db -> Lens s -> IO ()
setup db (Prim :: Lens s) =
  do
    bld <- buildCreateTblIfne db $ Table name cols key
    print bld
    execute db bld
  where
    fds = recover @(Fds s) Proxy
    key = do
      (left, right) <- Maybe.listToMaybe fds
      let cols = Set.union (Set.fromList left) (Set.fromList right)
      let coversAll = Set.isSubsetOf (Set.fromList colNames) cols
      if coversAll then Just left else Nothing
    name = head $ recover_tables @(Ts s) Proxy
    cols = map (uncurry Column) $ recover_env @(Rt s) Proxy
    colNames = map colName cols
setup db (Debug l) = setup db l
setup db (Join _ l1 l2) =
  do
    setup db l1
    setup db l2
setup db (Select _ l) = setup db l
setup db (Drop _ _ l) = setup db l
setup _ _ = error "Unsupported lens type for setup"
