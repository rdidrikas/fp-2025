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
  CombineAdd MealBody MealBody
  deriving Show

data Command =
  Meal MealBody |
  Add Food Amount Unit Calories MealType |
  Remove Food Amount Unit Calories MealType |
  Total Date |
  Display Date |
  Dump Dumpable
  deriving Show


examples :: [Command]
examples = [
    Remove (Food "lettuce") 1 Kilograms 100 Lunch,
    Add (Food "tea") 734 Milliliters 50 Dinner,
    Meal (CombineAdd (SingleAdd (Food "tomato") 38 Grams 85 Dinner) (SingleAdd (Food "tea") 7342 Milliliters 5 Dinner)),
    Display (Date { year = 2023, month = 3, day = 15 }),
    Total (Date{year = 2010,month = 09,day = 09}),
    Dump Examples
    ]