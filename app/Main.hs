module Main where

import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import Control.Exception (catch, IOException)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filepath] -> processFile filepath
    _ -> do
      progName <- getProgName
      putStrLn $ "Usage: " ++ progName ++ " <filepath>"
      exitFailure


-- | Attempt to read and process a file, handling errors gracefully
processFile :: FilePath -> IO ()
processFile filepath = do
  result <- readFileSafe filepath
  case result of
    Left err -> do
      hPutStrLn stderr ("Error: " ++ err)
      exitFailure
    Right contents -> do
      let lineCount = length (lines contents)
      putStrLn ("Read " ++ show lineCount ++ " lines from: " ++ filepath)

-- | Safely read a file, return either an error message or the contents
readFileSafe :: FilePath -> IO (Either String String)
readFileSafe filepath =
    (Right <$> readFile filepath) `catch` handleIOError
  where
    handleIOError :: IOException -> IO (Either String String)
    handleIOError _ = return (Left ("Cannot read the file: " ++ filepath))
