module Console where

import System.Console.Haskeline

repl :: (String -> String) -> IO ()
repl eval = runInputT defaultSettings loop
          where loop = do maybeLine <- getInputLine "user> "
                          case maybeLine of
                            Nothing -> return ()
                            Just line -> do
                              outputStrLn $ eval line
                              loop
        

