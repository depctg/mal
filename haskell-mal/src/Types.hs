module Types where

import Control.Monad.State
import qualified Data.Map.Lazy as M

-- Basic language type
data SExpression = List (Char, Char) [SExpression]
    | Symbol String
    | Keyword String
    | String String
    | Char Char
    | Number Int
    | Bool Bool
    deriving ( Eq, Ord )

instance Show SExpression where
    show (List (s, e) xs) = [s] ++ unwords (map show xs) ++ [e]
    show (Keyword s) = s
    show (Symbol s) = s
    show (String s) = show s
    show (Char c) = show c
    show (Number n) = show n
    show (Bool True) = "true"
    show (Bool False) = "false"

-- Runtime lisp types

-- Lisp values

data LispType = GenericType
              | FoldType LispType
              | SymbolType | NumberType | StringType | CharType | ListType | BoolType
              | FunctionType ([LispType], LispType)
              deriving ( Eq )

-- Lisp runtime type, state and Error handling

type LispRuntime a = StateT LispEnv (Either String) a

data EnvValue = LispValue        { ltype :: LispType, lexpr :: SExpression }
    | ExternelFunction { ltype :: LispType, lexpr :: SExpression, lfunc :: ([SExpression] -> SExpression) }
    | LispFunction     { ltype :: LispType, lexpr :: SExpression, lparam :: [SExpression], lenv :: LispEnv }

instance Show EnvValue where
    show (LispFunction _ _ _ _) = "#<function>"
    show t = show $ lexpr t

data EnvEntry = EnvEntry { lvalue :: EnvValue }

data LispEnv = LispEnv { getEnv :: M.Map SExpression EnvEntry , outerEnv :: LispEnv }
             | RootEnv



