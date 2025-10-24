{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Lib2(
    parseCommand
    , ToCliCommand(..)
    , process) where

import qualified Lib1
import Data.Char (isDigit, isSpace, isAlpha)
import Lib1 (MealBody(CombineAdd))

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
      Left _ -> case parseKeyword "Milliliters" input of
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

-- <meal_body> ::= <add> | <meal_body> ", " <meal_body>
parseMealBody :: Parser Lib1.MealBody
parseMealBody input = 
  case singleAdd input of
  Left err -> Left err
  Right (first, rest) -> -- keeps parsing if there is more
    case parseKeyword ", " rest of
      Left _ -> Right (first, rest)  -- if no more, return single add
      Right (_, rest2) ->            -- if more, parse continues
        case parseMealBody rest2 of  -- recursion
          Left err -> Left err
          Right (second, rest3) -> Right (Lib1.CombineAdd first second, rest3)
  where
    singleAdd input =
      case parseAdd input of
          Left err -> Left err
          Right (Lib1.Add food amount unit calories mealType, rest) ->
            Right (Lib1.SingleAdd food amount unit calories mealType, rest)
          Right (_, rest) -> Left "Expected add command"

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

-- Parse number
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
  `orElse` parseRemove
  `orElse` parseMeal

-- ==================== COMMAND PARSERS ====================

-- <dump_examples> ::= "dump examples"
parseDumpExamples :: Parser Lib1.Command
parseDumpExamples input =
  case parseKeyword "dump examples" input of
    Left err -> Left err -- if parsing failed this calls
    Right (_, rest) -> Right (Lib1.Dump Lib1.Examples, rest)

-- <display> ::= "display " <date>
parseDisplay :: Parser Lib1.Command
parseDisplay input =
  case parseKeyword "display" input of -- not using and 3 for readability (in my opinion looks better)
    Left err -> Left err
    Right (_, rest) ->
      case and2 parseWhitespace parseDate rest of
        Left err -> Left err
        Right ((_, date), rest2) -> Right (Lib1.Display date, rest2)

-- <total_calories> ::= "total " <date> 
parseTotal :: Parser Lib1.Command
parseTotal input =
  case parseKeyword "total" input of
    Left err -> Left err
    Right (_, rest) -> 
      case and2 parseWhitespace parseDate rest of
        Left err -> Left err
        Right ((_, date), rest2) -> Right (Lib1.Total date, rest2)

-- <add> ::= "add " <data> " to " <meal_type>
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

-- <remove> ::= "remove " <data> " from " <meal_type>
parseRemove :: Parser Lib1.Command
parseRemove input = 
  case and4 (parseKeyword "remove")
    (parseWhitespace `andThen` parseFoodData)
    (parseKeyword " from ")
    parseMealType input of
      Left err -> Left err
      Right ((_, foodData, _, mealType), rest) ->
        let (food, amount, unit, calories) = foodData
        in Right (Lib1.Remove food amount unit calories mealType, rest)

-- <meal> ::= "meal " <meal_body>
parseMeal :: Parser Lib1.Command
parseMeal input = 
  case and3 (parseKeyword "meal")
    parseWhitespace
    parseMealBody input of
      Left err -> Left err
      Right ((_, _, mealBody), rest) -> Right (Lib1.Meal mealBody, rest)

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
  toCliCommand (Lib1.Display date) = 
    "display " ++ show (Lib1.year date) ++ " " ++ show (Lib1.month date) ++ " " ++ show (Lib1.day date)
  toCliCommand (Lib1.Total date) = 
    "total " ++ show (Lib1.year date) ++ " " ++ show (Lib1.month date) ++ " " ++ show (Lib1.day date)
  toCliCommand (Lib1.Add food amount unit calories mealType) = "add " ++ toCliCommand food ++ ", " ++ show amount ++ " " ++ toCliCommand unit ++ ", " 
    ++ show calories ++ " to " ++ toCliCommand mealType
  toCliCommand (Lib1.Remove food amount unit calories mealType) = 
    "remove " ++ toCliCommand food ++ ", " ++ show amount ++ " " ++ toCliCommand unit ++ ", " ++ show calories ++ " from " ++ toCliCommand mealType
  toCliCommand (Lib1.Meal mealBody) = "meal " ++ toCliCommand mealBody

instance ToCliCommand Lib1.MealBody where
  toCliCommand :: MealBody -> String
  toCliCommand (Lib1.SingleAdd food amount unit calories mealType) =
    toCliCommand (Lib1.Add food amount unit calories mealType)
  
  toCliCommand (Lib1.CombineAdd first second) =
    toCliCommand first ++ ", " ++ toCliCommand second

instance ToCliCommand Lib1.Food where
  toCliCommand :: Lib1.Food -> String
  toCliCommand (Lib1.Food name) = name

instance Eq Lib1.Command where
  (==) :: Lib1.Command -> Lib1.Command -> Bool
  Lib1.Dump Lib1.Examples == Lib1.Dump Lib1.Examples = True
  Lib1.Display date1 == Lib1.Display date2 = date1 == date2
  Lib1.Add f1 a1 u1 c1 m1 == Lib1.Add f2 a2 u2 c2 m2 =
    f1 == f2 && a1 == a2 && u1 == u2 && c1 == c2 && m1 == m2
  Lib1.Remove f1 a1 u1 c1 m1 == Lib1.Remove f2 a2 u2 c2 m2 =
    f1 == f2 && a1 == a2 && u1 == u2 && c1 == c2 && m1 == m2
  Lib1.Total date1 == Lib1.Total date2 = date1 == date2
  Lib1.Meal body1 == Lib1.Meal body2 = body1 == body2
  _ == _ = False -- everything else is not equal

instance Eq Lib1.Food where
  (==) :: Lib1.Food -> Lib1.Food -> Bool
  Lib1.Food food1 == Lib1.Food food2 = food1 == food2

instance Eq Lib1.MealBody where
  (==) :: Lib1.MealBody -> Lib1.MealBody -> Bool
  Lib1.SingleAdd f1 a1 u1 c1 m1 == Lib1.SingleAdd f2 a2 u2 c2 m2 =
    f1 == f2 && a1 == a2 && u1 == u2 && c1 == c2 && m1 == m2
  Lib1.CombineAdd a1 b1 == Lib1.CombineAdd a2 b2 = 
    a1 == a2 && b1 == b2
  _ == _ = False

instance Eq Lib1.Date where
  (==) :: Lib1.Date -> Lib1.Date -> Bool
  Lib1.Date y1 m1 d1 == Lib1.Date y2 m2 d2 = 
    y1 == y2 && m1 == m2 && d1 == d2    

instance ToCliCommand Lib1.Unit where
  toCliCommand Lib1.Grams = "Grams"
  toCliCommand Lib1.Kilograms = "Kilograms"
  toCliCommand Lib1.Milliliters = "Milliliters"
  toCliCommand Lib1.Liters = "Liters"

instance ToCliCommand Lib1.MealType where
  toCliCommand Lib1.Breakfast = "breakfast"
  toCliCommand Lib1.Lunch = "lunch"
  toCliCommand Lib1.Dinner = "dinner"
  toCliCommand Lib1.Snack = "snack"

instance Eq Lib1.Unit where
  (==) :: Lib1.Unit -> Lib1.Unit -> Bool
  Lib1.Grams == Lib1.Grams = True
  Lib1.Kilograms == Lib1.Kilograms = True
  Lib1.Milliliters == Lib1.Milliliters = True
  Lib1.Liters == Lib1.Liters = True
  _ == _ = False

instance Eq Lib1.MealType where
  (==) :: Lib1.MealType -> Lib1.MealType -> Bool
  Lib1.Breakfast == Lib1.Breakfast = True
  Lib1.Lunch == Lib1.Lunch = True
  Lib1.Dinner == Lib1.Dinner = True
  Lib1.Snack == Lib1.Snack = True
  _ == _ = False