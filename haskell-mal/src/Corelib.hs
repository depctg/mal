module Corelib
    ( baseEnv
    ) where

import Types
import qualified Data.Map.Lazy as M

defaultBrucket = ('(', ')')

-- define helper functions and primitives
unaryListOp out s op = EnvEntry $ ExternelFunction (FunctionType ([ListType], out)) (Symbol s) op
binaryNumberOp s op = EnvEntry $ ExternelFunction (FunctionType ([NumberType, NumberType], NumberType)) (Symbol s) op
binaryGenericOp s op = EnvEntry $ ExternelFunction (FunctionType ([GenericType, GenericType], GenericType)) (Symbol s) op
 
ladd [(Number a), (Number b)]  = Number $ a + b
lsub [(Number a), (Number b)]  = Number $ a - b
lmult [(Number a), (Number b)] = Number $ a * b
ldiv [(Number a), (Number b)]  = Number $ a `div` b
lcar [(List _ xs)]             = head xs
leq [x, y]                     = Bool $ x == y
llist xs                       = List defaultBrucket $ xs
lgt [x, y]                     = Bool $ x > y
lge [x, y]                     = Bool $ x >= y
llt [x, y]                     = Bool $ x < y
lle [x, y]                     = Bool $ x <= y
lcount [(List _ xs)]           = Number $ length xs

lislist [(List _ _)]           = Bool $ True
lislist _                      = Bool $ False
lisempty [(List _ [])]         = Bool $ True
lisempty _                     = Bool $ False

primitives = [  (Symbol "nil", EnvEntry $ LispValue ListType (List defaultBrucket [])),
                (Symbol "+", binaryNumberOp "+" ladd),
                (Symbol "-", binaryNumberOp "-" lsub),
                (Symbol "*", binaryNumberOp "*" lmult),
                (Symbol "/", binaryNumberOp "/" ldiv),
                (Symbol "=", binaryGenericOp "=" leq),
                (Symbol ">", binaryGenericOp ">" lgt),
                (Symbol ">=", binaryGenericOp ">=" lge),
                (Symbol "<", binaryGenericOp "<" llt),
                (Symbol "<=", binaryGenericOp "<=" lle),
                (Symbol "list", unaryListOp ListType "list" llist ),
                (Symbol "list?", unaryListOp BoolType "list?" lislist ),
                (Symbol "count", unaryListOp BoolType "count" lcount ),
                (Symbol "empty?", unaryListOp ListType "empty?" lisempty  ) ]

-- basic env, core functions
baseEnv = LispEnv (M.fromList primitives) RootEnv

