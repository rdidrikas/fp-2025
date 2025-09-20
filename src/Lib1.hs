module Lib1
    ( examples, Command(..), Dumpable(..)
    ) where

data Dumpable = Examples
  deriving Show

-- This is a "root" ADT representing your grammar,
-- Please expand this ADT as needed

-- Top-level commands
data Command
    = meal MealBody
    | Add Data MealType
    | Remove Data MealType
    | TotalCalories Date
    | CurrentCalories
    | Goal GoalOption
    | Show Date
    | DumpExamples
    deriving (Show, Eq)

-- Meal can be recursive (one add, another meal, or nested meals)
data MealBody
    = SingleAdd Data MealType
    | MultipleMeals [MealBody]
    deriving (Show, Eq)

-- Data for food entries
data Data = Data
    { food     :: Food
    , amount   :: Int
    , unit     :: Unit
    , calories :: Int
    }
    deriving (Show, Eq)

-- Food options
data Food
    = Apple | Banana | Chicken | Rice | Oats
    | Milk | Egg | Bread | Salmon | Potato
    | Beef | Pork | Yogurt | Cheese | Butter
    | Tomato | Cucumber | Lettuce | Carrot | Onion
    | Orange | Pear | Grapes | Strawberry | Blueberry
    | Coffee | Tea | Water | Juice | Soda
    | Pizza | Pasta | Burger | Sandwich | Soup
    deriving (Show, Eq, Enum, Bounded)

data Unit = Gram | Kilogram | Milliliter | Liter
    deriving (Show, Eq)

-- Meal types
data MealType = Breakfast | Lunch | Dinner | Snack
    deriving (Show, Eq)

-- Dates
data Date = Date
    { year  :: Int
    , month :: Int
    , day   :: Int
    }
    deriving (Show, Eq)

-- Goal handling
data GoalOption
    = Set Date Int
    | ShowGoal Date
    | Compose [SingleGoal]
    deriving (Show, Eq)

data SingleGoal = SingleGoal Date Int
    deriving (Show, Eq)

data Command = Dump Dumpable
  deriving Show

examples :: [Command]
examples = [
    Dump Examples
    ]
