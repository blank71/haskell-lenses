module Lens.Predicate.Precedence where

import qualified Lens.Predicate.Base as P

data Op = Or | And | Cmp | Not | Add | Sub | Mult | Divide deriving (Eq, Ord)

first :: Op
first = Or

ofOp :: P.Operator -> Op
ofOp P.LogicalAnd = And
ofOp P.LogicalOr = Or
ofOp P.Plus = Add
-- ofOp P.Minus = Sub
ofOp P.Equal = Cmp
ofOp P.GreaterThan = Cmp
ofOp P.LessThan = Cmp

-- ofOp P.Multiply = Mult
-- ofOp P.Divide = Divide

ofUnaryOp :: P.UnaryOperator -> Op
ofUnaryOp _ = Not
