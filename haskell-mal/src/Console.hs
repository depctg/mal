module Console where

import System.Console.Readline ( readline )

eval :: String -> String
eval = id

repl :: IO ()
repl = do
  maybeLine <- readline "user> "
  case maybeLine of
    Nothing -> return ()
    Just line -> do
      putStrLn line
      repl
        

