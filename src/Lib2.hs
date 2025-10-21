
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Lib2(
    parseCommand
    , ToCliCommand(..)
    , process) where

import qualified Lib1
import Data.Char (isDigit, isSpace, isAlpha)

type ErrorMsg = String
type Parser a = String -> Either ErrorMsg (a, String)


-- ==================== PARSER COMBINATORS ====================

orElse :: Parser a -> Parser a -> Parser a
orElse p1 p2 input
  = case p1 input of
      Right result -> Right result
      Left _ -> p2 input

-- Helper for keeping only the second result
andThen :: Parser a -> Parser b -> Parser b
andThen p1 p2 input =
  case p1 input of
    Left err -> Left err
    Right (_, rest) -> p2 rest

and2 :: Parser a -> Parser b -> Parser (a, b)
and2 p1 p2 input
  = case p1 input of
      Left err -> Left err
      Right (a, rest1)
        -> case p2 rest1 of
             Left err -> Left err
             Right (b, rest2) -> Right ((a, b), rest2)

and3 :: Parser a -> Parser b -> Parser c -> Parser (a, b, c)
and3 p1 p2 p3 input
  = case and2 p1 p2 input of
      Left err -> Left err
      Right ((a, b), rest)
        -> case p3 rest of
             Left err -> Left err
             Right (c, rest2) -> Right ((a, b, c), rest2)

and4 :: Parser a -> Parser b -> Parser c -> Parser d -> Parser (a, b, c, d)
and4 p1 p2 p3 p4 input =
  case and3 p1 p2 p3 input of
    Left err -> Left err
    Right ((a, b, c), rest) ->
      case p4 rest of
        Left err -> Left err
        Right (d, rest2) -> Right ((a, b, c, d), rest2)

and5 :: Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser (a, b, c, d, e)
and5 p1 p2 p3 p4 p5 input =
  case and4 p1 p2 p3 p4 input of
    Left err -> Left err
    Right ((a, b, c, d), rest) ->
      case p5 rest of
        Left err -> Left err
        Right (e, rest2) -> Right ((a, b, c, d, e), rest2)

-- ==================== OTHER HELPERS ====================

-- <unit> ::= "Grams" | "Kilograms" | "Mililiters" | "Liters"
parseUnit :: Parser Lib1.Unit
parseUnit input =
  case parseKeyword "Grams" input of
    Right (_, rest) -> Right (Lib1.Grams, rest)
    Left _ -> case parseKeyword "Kilograms" input of
      Right (_, rest) -> Right (Lib1.Kilograms, rest)
      Left _ -> case parseKeyword "Mililiters" input of
        Right (_, rest) -> Right (Lib1.Milliliters, rest)
        Left _ -> case parseKeyword "Liters" input of
          Right (_, rest) -> Right (Lib1.Liters, rest)
          Left err -> Left err

-- <meal_type> ::= "breakfast" | "lunch" | "dinner" | "snack"
parseMealType :: Parser Lib1.MealType
parseMealType input =
  case parseKeyword "breakfast" input of
    Right (_, rest) -> Right (Lib1.Breakfast, rest)
    Left _ -> case parseKeyword "lunch" input of
      Right (_, rest) -> Right (Lib1.Lunch, rest)
      Left _ -> case parseKeyword "dinner" input of
        Right (_, rest) -> Right (Lib1.Dinner, rest)
        Left _ -> case parseKeyword "snack" input of
          Right (_, rest) -> Right (Lib1.Snack, rest)
          Left err -> Left err

-- <data> ::= <food> ", " <amount> <unit> ", " <calories> " "
parseFoodData :: Parser (Lib1.Food, Int, Lib1.Unit, Int)
parseFoodData input =
  case and4 parseWord -- Food name
    (parseComma `andThen` parseNumber) -- Amount
    (parseWhitespace `andThen` parseUnit)  -- Unit with space
    (parseComma `andThen` parseNumber) input of
      Left err -> Left err
      Right ((foodName, amount, unit, calories), rest) ->
        Right ((Lib1.Food foodName, amount, unit, calories), rest)

-- <date> ::= <year> " " <month> " " <day>
parseDate :: Parser Lib1.Date
parseDate input =
  case and5 parseNumber parseWhitespace parseNumber parseWhitespace parseNumber input of
    Left err -> Left err
    Right ((year, _, month, _, day), rest) ->  -- remove whitespace results
      Right (Lib1.Date year month day, rest)

-- ==================== BASIC PARSER HELPERS ====================

-- Helper to parse a specific string
parseWord :: Parser String
parseWord input =
  let (word, rest) = span isAlpha input
  in if null word
     then Left "Expected word"
     else Right (word, rest)

-- Helper to check if a string starts with another string
isPrefixOf :: String -> String -> Bool
isPrefixOf prefix str = prefix == take (length prefix) str

-- Parse number with n
parseNumber :: Parser Int
parseNumber input =
  let (digits, rest) = span isDigit input
  in if null digits 
     then Left "Expected number"
     else Right (read digits, rest)

-- Parse whitespace
parseWhitespace :: Parser String
parseWhitespace input
  = case span isSpace input of
      ("", _) -> Left "Expected whitespace"
      (spaces, rest) -> Right (spaces, rest)

-- For command parsing
parseKeyword :: String -> Parser String
parseKeyword expected input
  = if expected `isPrefixOf` input then
        Right (expected, drop (length expected) input)
    else
        Left $ "Expected '" ++ expected ++ "'"

-- Helper to parse comma + space
parseComma :: Parser String
parseComma = parseKeyword ", "

-- ==================== MAIN PARSER ====================

-- | Parses user's input
parseCommand :: Parser Lib1.Command
parseCommand = parseDumpExamples 
  `orElse` parseDisplay
  `orElse` parseTotal
  `orElse` parseAdd

-- ==================== COMMAND PARSERS ====================

-- <dump_examples> ::= "dump examples"
parseDumpExamples :: Parser Lib1.Command
parseDumpExamples input =
  case parseKeyword "dump examples" input of
    Left err -> Left err -- if parsing failed this calls
    Right (_, rest) -> Right (Lib1.Dump Lib1.Examples, rest)

parseDisplay :: Parser Lib1.Command
parseDisplay input =
  case parseKeyword "display" input of -- not using and 3 for readability (in my opinion looks better)
    Left err -> Left err
    Right (_, rest) ->
      case and2 parseWhitespace parseDate rest of
        Left err -> Left err
        Right ((_, date), rest2) -> Right (Lib1.Display date, rest2)

parseTotal :: Parser Lib1.Command
parseTotal input =
  case parseKeyword "total" input of
    Left err -> Left err
    Right (_, rest) -> 
      case and2 parseWhitespace parseDate rest of
        Left err -> Left err
        Right ((_, date), rest2) -> Right (Lib1.Total date, rest2)
  
parseAdd :: Parser Lib1.Command
parseAdd input =
    case and4 (parseKeyword "add")
      (parseWhitespace `andThen` parseFoodData)
      (parseKeyword " to ")
      parseMealType input of
        Left err -> Left err
        Right ((_, foodData, _, mealType), rest) -> 
          let (food, amount, unit, calories) = foodData
          in Right (Lib1.Add food amount unit calories mealType, rest)

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
  toCliCommand (Lib1.Display date) = "display " ++ show date
  toCliCommand (Lib1.Total date) = "total " ++ show date
  toCliCommand (Lib1.Add food amount unit calories mealType) = "add " ++ show food ++ ", " ++ show amount ++ " " ++ toCliCommand unit ++ ", " 
    ++ show calories ++ " to " ++ toCliCommand mealType
  toCliCommand cmd = "Example: " ++ show cmd

instance Eq Lib1.Command where
  (==) :: Lib1.Command -> Lib1.Command -> Bool
  Lib1.Dump Lib1.Examples == Lib1.Dump Lib1.Examples = True
  -- Lib1.Display date1 == Lib1.Display date2 = date1 == date2 -- probably will need later
  _ == _ = False -- everything else is not equal

{- Also may need later
instance Eq Lib1.Date where
  (==) :: Lib1.Date -> Lib1.Date -> Bool
  Lib1.Date y1 m1 d1 == Lib1.Date y2 m2 d2 = 
    y1 == y2 && m1 == m2 && d1 == d2    
-}

instance ToCliCommand Lib1.Unit where
  toCliCommand Lib1.Grams = "Grams"
  toCliCommand Lib1.Kilograms = "Kilograms"
  toCliCommand Lib1.Milliliters = "Mililiters"
  toCliCommand Lib1.Liters = "Liters"

instance ToCliCommand Lib1.MealType where
  toCliCommand Lib1.Breakfast = "breakfast"
  toCliCommand Lib1.Lunch = "lunch"
  toCliCommand Lib1.Dinner = "dinner"
  toCliCommand Lib1.Snack = "snack"
