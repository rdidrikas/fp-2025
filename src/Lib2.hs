
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Lib2(
    parseCommand
    , ToCliCommand(..)
    , process) where

import qualified Lib1

type ErrorMsg = String
type Parser a = String -> Either ErrorMsg (a, String)


-- ==================== BASIC PARSER HELPERS ====================

-- Helper to parse a specific string
parseString :: String -> Parser String
parseString expected = \input ->
  if expected `isPrefixOf` input
    then Right (expected, drop (length expected) input)
    else Left $ "Expected '" ++ expected ++ "'"

-- Helper to check if a string starts with another string
isPrefixOf :: String -> String -> Bool
isPrefixOf prefix str = prefix == take (length prefix) str

-- ==================== MAIN PARSER ====================

-- | Parses user's input
parseCommand :: Parser Lib1.Command -- returns either
parseCommand input = parseDumpExamples input

-- <dump_examples> ::= "dump examples"
parseDumpExamples :: Parser Lib1.Command
parseDumpExamples input =
  case parseString "dump examples" input of
    Left err -> Left err -- if parsing failed this calls
    Right (_, rest) -> Right (Lib1.Dump Lib1.Examples, rest)

-- ==================== TYPE CLASS INSTANCES ====================

process :: Lib1.Command -> [String]
process (Lib1.Dump Lib1.Examples) = 
  "Examples:" : map toCliCommand Lib1.examples
process c = ["Parsed as: " ++ show c]

class ToCliCommand a where
  toCliCommand :: a -> String

instance ToCliCommand Lib1.Command where
  toCliCommand :: Lib1.Command -> String
  toCliCommand (Lib1.Dump Lib1.Examples) = "dump examples"
  toCliCommand cmd = "Example: " ++ show cmd

instance Eq Lib1.Command where
  (==) :: Lib1.Command -> Lib1.Command -> Bool
  Lib1.Dump Lib1.Examples == Lib1.Dump Lib1.Examples = True
  _ == _ = False