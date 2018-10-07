module Main where

import Lisp ( parseLine, evalBase )
import Console ( repl )

main :: IO ()
main = repl $ \s -> case parseLine s of
                      Left err   -> show err
                      Right expr -> case evalBase expr of
                                      Left err -> err
                                      Right expr -> show expr
