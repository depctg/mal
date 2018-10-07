module Main where

import Lisp ( parseLine, evalLineIO )
import Console ( replIO )

main :: IO ()
main = replIO evalLineIO
