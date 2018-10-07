{-# LANGUAGE GADTs #-}

module Lisp where

import Text.Parsec
import Data.Maybe (fromMaybe)
import qualified Data.Map.Lazy as M
import Control.Monad.State
import Control.Monad.Except (liftEither, throwError)
import Data.IORef
import Utils
import Corelib (baseEnv)
import Types

-- syntax

symbolBeginChars = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['+', '-', '*', '/', ':', '=', '>', '<']
symbolFollowingChars = symbolBeginChars ++ ['0' .. '9'] ++ ['!', '?'] 

symbol :: Parsec String () SExpression
symbol = Symbol <$> pSymbol
           where pSymbol = (:) <$> oneOf symbolBeginChars <*> many (oneOf symbolFollowingChars)

escapedChars = "\"\\"
controlChars = escapedChars ++ "tn"

atom :: Parsec String () SExpression
atom = try $ Number . read <$> ((:) <$> option ' ' (char '-') <*> many1 digit)
   <|> String <$> (char '"' *> many stringElem <* char '"')
   <|> Char <$> (char '\'' *> anyChar <* char '\'')
   <|> Bool . const True <$> string "true"
   <|> Bool . const False <$> string "false"
        where
            stringElem = (char '\\' *> oneOf controlChars) <|> noneOf escapedChars

keywords = ["if", "else", "let*", "lambda", "fn*"]

keyword :: Parsec String () SExpression
keyword = Keyword <$> choice (map (try . string) keywords)

quoteSymbols = [("'", "quote"),
                ("`", "quasiquote"),
                ("~@", "splice-unquote"),
                ("~", "unquote"),
                ("@", "deref")]

quoted = choice $ map parseQuoted quoteSymbols
           where parseQuoted (sym, kw) = try $ do string sym
                                                  expr <- sExpression
                                                  return $ List defaultBrucket [Keyword kw, expr]

defaultBrucket = ('(', ')')
bruckets = defaultBrucket : [('{', '}'), ('[', ']')]

sExpression :: Parsec String () SExpression
sExpression = try quoted <|> term <|> choice (map list bruckets)
            where term = atom <|> try keyword <|> symbol
                  sep = skipMany1 (char ',' <|> space)
                  list ss@(s, e) = List <$> pure ss <*> between (char s) (char e) (optional sep *> sepEndBy sExpression sep)


comment :: Parsec String () String
comment = (:) <$> char ';' <*> many anyChar

line :: Parsec String () SExpression
line = do maybeMeta <- optionMaybe meta
          spaces
          expr <- sExpression
          spaces
          optional comment
          case maybeMeta of
            Nothing -> return $ expr
            Just m -> return $ List defaultBrucket [Keyword "with-meta", expr, m]
       where meta = char '^' *> sExpression

-- env & eval & apply


-- Util
applyTypeCheck :: LispType -> [LispType] -> LispRuntime LispType
applyTypeCheck (FunctionType (args, ret)) ts = if args == ts then return ret else throwError "Type mismatch"


modifyEnv :: (M.Map SExpression EnvEntry -> M.Map SExpression EnvEntry) -> LispEnv -> LispEnv
modifyEnv f (LispEnv env out) = LispEnv (f env) out
modifyEnv _ a = a

envSearch :: SExpression -> LispRuntime EnvValue
envSearch sym@(Symbol s) = do
    res <- gets search
    liftEither $ maybeToEither ("Symbol not found: " ++ s) $ lvalue <$> res
    where search (LispEnv e out) = M.lookup sym e `orElse` search out
          search RootEnv = Nothing
envSearch _ = throwError "Can not search non-symbol values"

envSet :: SExpression -> EnvValue -> LispRuntime ()
envSet s v = (modify $ modifyEnv $ M.insert s (EnvEntry v))

-- desugar the lambda to let rec
-- TODO: check # of params, args, and if symbol
withNewEnv param arg action outer = do
    out <- get
    put $ LispEnv M.empty outer
    sequence_ $ zipWith envSet param arg
    action <* put out
withSubEnv :: [SExpression] -> [EnvValue] -> LispRuntime a -> LispRuntime a
withSubEnv param arg action = get >>= withNewEnv param arg action
    

-- convert expression into value, deal with special formats
eval :: SExpression -> LispRuntime EnvValue
eval sym@(Symbol _) = envSearch sym
eval (List p [Symbol "def!", k, v]) = do
    value <- eval v
    envSet k value
    return value
eval (List p [Keyword "let*", (List _ kvs), v]) = do
    let setops = zipWith (\k v -> eval v >>= envSet k) (evens kvs) (odds kvs) 
    withSubEnv [] [] $ sequence_ setops >> eval v
eval (List p [Keyword "if", cond, then_expr, else_expr]) = do
    bcond <- eval cond
    case lexpr bcond of
        Bool False -> eval else_expr
        _ -> eval then_expr
eval (List p [Keyword "if", cond, then_expr]) = do
    bcond <- eval cond
    case lexpr bcond of
        Bool False -> return $ LispValue ListType $ List defaultBrucket []
        _ -> eval then_expr
eval (List _ [Keyword "fn*", (List _ params), body]) = get >>= return . LispFunction StringType body params
eval (List p xs) = mapM eval xs >>= apply p
eval e@(String s) = return $ LispValue StringType e
eval e@(Char s) = return $ LispValue CharType e
eval e@(Number s) = return $ LispValue NumberType e
eval e@(Bool b) = return $ LispValue BoolType e
eval e = throwError $ "unknow Expression " ++ show e

apply :: (Char, Char) -> [EnvValue] -> LispRuntime EnvValue
apply _ ((ExternelFunction t _ f):ps) = do
    -- retType <- applyTypeCheck t (map ltype ps)
    let expr = f (map lexpr ps)
    return $ LispValue GenericType expr
-- In lisp we only type check primitive functions
apply _ ((LispFunction t body ps env):as) = withNewEnv ps as (eval body) env
-- for now we keep eval, but in real word we just exit
apply p s = return $ LispValue ListType $ List p $ map lexpr s

-- export functions & IO Monad Stateful runtime

parseLine = parse line "REPL"

evalBase p = evalStateT (eval p) baseEnv

evalLineIO :: IORef (Maybe (LispEnv)) -> String -> IO String
evalLineIO state str = do
    menv <- readIORef state
    let env = maybe baseEnv id menv
    let p = parseLine str
    case p of
        Left err -> return $ show err
        Right ast -> case runStateT (eval ast) env of
                        Left err -> return $ show err
                        Right (value, newenv) -> do
                            writeIORef state $ Just newenv
                            return $ show value


