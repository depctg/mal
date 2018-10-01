module Console where

import System.Console.Readline ( readline )

repl :: (String -> String) -> IO ()
repl eval = do
  maybeLine <- readline "user> "
  case maybeLine of
    Nothing -> return ()
    Just line -> do
      putStrLn $ eval line
      repl eval
        

