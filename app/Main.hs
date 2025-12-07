module Main where

import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filepath] -> do
      putStrLn $ "The file to validate is: " ++ filepath
    _ -> do
      progName <- getProgName
      putStrLn $ "Usage: " ++ progName ++ " <filepath>"
      exitFailure
