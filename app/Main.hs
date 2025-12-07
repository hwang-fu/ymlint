module Main where

import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure, exitSuccess)
import Control.Exception (catch, IOException)
import System.IO (hPutStrLn, stderr)

import Yaml.Lexer (Token(..), tokenize)

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
    Right contents -> validateYaml contents

-- | Safely read a file, return either an error message or the contents
readFileSafe :: FilePath -> IO (Either String String)
readFileSafe filepath =
    (Right <$> readFile filepath) `catch` handleIOError
  where
    handleIOError :: IOException -> IO (Either String String)
    handleIOError _ = return (Left ("Cannot read the file: " ++ filepath))

validateYaml :: String -> IO ()
validateYaml contents = case findErrors (tokenize contents) of
  [] -> do
    putStrLn "This is a valid YAML file."
    exitSuccess
  errors -> do
    mapM_ reportError errors
    exitFailure

findErrors :: [Token] -> [(Int, String)]
findErrors tokens =
    [(lineNr, content) | (lineNr, Invalid content) <- indexedTokens]
  where
    indexedTokens = zip [1..] tokens

reportError :: (Int, String) -> IO ()
reportError (lineNr, content) = hPutStrLn stderr ("Line " ++ show lineNr ++ ": Invalid syntax: " ++ content)
