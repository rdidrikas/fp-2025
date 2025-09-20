module Lib1
    ( examples, Command(..), Dumpable(..)
    ) where

data Dumpable = Examples
  deriving Show

-- This is a "root" ADT representing your grammar,
-- Please expand this ADT as needed

examples :: [Command]
examples = [
    "meal add tomato, 38g, 85 kcal to dinner, add tea, 7342ml, 5 kcal to dinner, meal add lettuce, 0kg, 4 kcal to dinner, add sandwich, 9kg, 94 kcal to dinner, add strawberry, 5ml, 4 kcal to lunch",
    "meal add tomato, 56kg, 1 kcal to breakfast, add onion, 100g, 400 kcal to snack, add cucumber, 100g, 90 kcal to snack",
    "goal compose 2025-08-28, 600, 2025-02-17, 3000, 2025-02-07, 1000",
    "total 2014-06-23"
    ]
