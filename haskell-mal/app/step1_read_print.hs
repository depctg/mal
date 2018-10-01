module Main where

import Lib
import Lisp ( parseLine )
import Console ( repl )

main :: IO ()
main = repl $ \s -> case parseLine s of
                      Left err   -> show err
                      Right expr -> show expr
