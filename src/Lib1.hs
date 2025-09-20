module Lib1
    ( examples, Command(..), Dumpable(..)
    ) where

data Dumpable = Examples
  deriving Show

-- This is a "root" ADT representing your grammar,
-- Please expand this ADT as needed

examples :: [Command]
examples = [
    "meal add lettuce, 7g, 5 kcal to lunch, add sandwich, 87l, 6 kcal to dinner, 
    add cucumber, 8ml, 7 kcal to dinner, 
    add bread, 8kg, 8 kcal to breakfast, 
    add pork, 85kg, 5 kcal to snack",
    "meal add tomato, 056l, 1 kcal to breakfast, add onion, 4l, 4 kcal to snack",
    "goal compose 2025-08-28, 600, 2025-02-17, 3000, 2025-02-07, 1000",
    "total 2014-06-23"
    ]
