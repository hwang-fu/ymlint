module Yaml.Lexer
  ( Token(..)
  , tokenizeLine
  , tokenize
  ) where

data Token
  = Blank                            -- ^ Empty or whitesapce only line
  | Comment String                   -- ^ Comment
  | Dictionary Int String String     -- ^ Key-value pair (indent, key, value)
  | ListItem Int String              -- ^ List item (indent content)
  | MultiLine Int Char               -- ^ Multi-line marker (indent, | or >)
  | Invalid String                   -- ^ Unrecognized line
  deriving (Show, Eq)

tokenize :: String -> [Token]
tokenize = map tokenizeLine . lines

tokenizeLine :: String -> Token
tokenizeLine line
    | all isSpace line      = Blank
    | isComment stripped    = Comment (extractComment stripped)
    | isListItem stripped   = ListItem indent (extractListContent stripped)
    | isMultiLine stripped  = MultiLine indent (extractMultiLineChar stripped)
    | isDictionary stripped = parseDictionary indent stripped
    | otherwise             = Invalid line
  where
    indent    = countIndent line
    stripped  = dropWhile isSpace line

isSpace :: Char -> Bool
isSpace c = (c == ' ') || c == '\t'

countIndent :: String -> Int
countIndent = length . takeWhile isSpace

isComment :: String -> Bool
isComment s = not (null s) && head s == '#'

extractComment :: String -> String
extractComment = drop 1 . dropWhile (== '#')

isListItem :: String -> Bool
isListItem s = case s of
  ('-' : ' '  : _) -> True
  ('-' : '\t' : _) -> True
  "-"              -> True
  _                -> False

extractListContent :: String -> String
extractListContent s = case s of
  ('-' : ' '  : rest) -> rest
  ('-' : '\t' : rest) -> rest
  "-"                 -> ""
  _                   -> s

isMultiLine :: String -> Bool
isMultiLine s = (s == "|") || (s == ">")

extractMultiLineChar :: String -> Char
extractMultiLineChar = head

isDictionary :: String -> Bool
isDictionary s =
    (':' `elem` s) && not (null key) && validKey key
  where
    key :: String
    key = takeWhile (/= ':') s
    validKey :: String -> Bool
    validKey k = not (null k) && all validKeyChar k
    validKeyChar :: Char -> Bool
    validKeyChar c = c `elem` (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "-")

parseDictionary :: Int -> String -> Token
parseDictionary indent s =
    Dictionary indent key value
  where
    (key, rest) = break (== ':') s
    value = dropWhile isSpace (drop 1 rest)
