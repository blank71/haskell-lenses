module Tables where

import Common
import Data.Type.Set
import GHC.TypeLits
import Label

type Tables = [Symbol]

type DisjointTables ts1 ts2 = OkOrError (IsDisjoint ts1 ts2) ('Text "The tables are not disjoint.")

class RecoverTables t where
  recover_tables :: Proxy t -> [String]

instance RecoverTables '[] where
  recover_tables Proxy = []

instance (RecoverTables xs, KnownSymbol x) => RecoverTables (x ': xs) where
  recover_tables Proxy = symbolVal (Proxy :: Proxy x) : recover_tables (Proxy :: Proxy xs)
