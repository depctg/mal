module Console where

import System.Console.Haskeline
import Data.IORef
import Control.Monad.IO.Class ( liftIO )

repl :: (String -> String) -> IO ()
repl eval = runInputT defaultSettings loop
          where loop = do maybeLine <- getInputLine "user> "
                          case maybeLine of
                            Nothing -> return ()
                            Just line -> do
                              outputStrLn $ eval line
                              loop

replIO :: (IORef (Maybe a) -> String -> IO String) -> IO ()
replIO eval = do
  state <- newIORef Nothing
  runInputT defaultSettings $ loop state
          where loop state = do maybeLine <- getInputLine "user> "
                                case maybeLine of
                                  Nothing -> return ()
                                  Just line -> do
                                    outputStrLn =<< (liftIO $ eval state line)
                                    loop state
        

