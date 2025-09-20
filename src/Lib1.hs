module Lib1
    ( examples, Command(..), Dumpable(..)
    ) where
--import Lib1 (Command)

data Dumpable = Examples
  deriving Show

-- This is a "root" ADT representing your grammar,
-- Please expand this ADT as needed

data Food = Food String
  deriving Show

data Unit = Grams | Kilograms | Milliliters | Liters
  deriving Show

data MealType = Breakfast | Lunch | Dinner | Snack
  deriving Show

type Amount = Int
type Calories = Int

data Date = Date
  { year  :: Int
  , month :: Int
  , day   :: Int
  } deriving Show

data MealBody = SingleAdd Food Amount Unit Calories MealType |
  NestedAdd [MealBody]
  deriving Show

data Command =
  Meal MealBody|
  Add Food Amount Unit Calories MealType |
  Remove Food Amount Unit Calories MealType |
  Total Date 
  deriving Show


examples :: [Command]
examples = [
    Add (Food "tomato") 38 Grams 85 Dinner,
    Add (Food "tea") 7342 Milliliters 5 Dinner,
    Meal (NestedAdd [SingleAdd (Food "tomato") 38 Grams 85 Dinner, SingleAdd (Food "tea") 7342 Milliliters 5 Dinner]),
    Add (Food "lettuce") 0 Kilograms 4 Dinner
    ]
