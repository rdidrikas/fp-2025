# Meal Manager

## Description

This DSL is designed for simple calorie tracking and meal management. You can add foods to meals, remove them, check totals, and manage goals.

## Entities & Operations

- **Entities:**
    - `Food`: A food item (apple, chicken, rice, coffee, etc.).
    - `Amount`: Quantity of food.
    - `Unit`: The measurement unit (grams, milliliters, etc.).
    - `Meal`: Multiple food entities
    - `Date`: A calendar date.
    - `Goal`: A calorie goal (daily or composite).
- **Operations:**
    - `add`: Add a food item to a meal.
    - `remove`: Remove a food item from a meal.
    - `meal`: Define a recursive meal structure (can contain foods or other meals).     
    - `total`: Show total calories for a given date.
    - `goal`: Set, show, or define composite goals.
    - `display`: Display food intake of your date of choice.

## BNF

```bnf
<command> ::= <meal> | <add> | <remove> | <total_calories> | <goal> | <display> | <dump_examples>

<meal> ::= "meal " <meal_body>
<meal_body> ::= <add> | <meal_body> ", " <meal_body>
<add> ::= "add " <data> "to " <meal_type>
<remove> ::= "remove " <data> "from " <meal_type>
<total_calories> ::= "total " <date> 
<goal> ::= "goal " <goal_option>

<data> ::= <food> ", " <amount> <unit> ", " <calories> " "
<food> ::= "apple" | "banana" | "chicken" | "rice" | "oats" | "milk" | "egg" | "bread" | "salmon" | "potato" | "beef" | "pork" | "yogurt" | "cheese" | "butter" | "tomato" | "cucumber" | "lettuce" | "carrot" | "onion" | "orange" | "pear" | "grapes" | "strawberry" | "blueberry"| "coffee" | "tea" | "water" | "juice" | "soda"| "pizza" | "pasta" | "burger" | "sandwich" | "soup"
<amount> ::= <digit> | <digit> <amount>
<unit> ::= "Grams" | "Kilograms" | "Mililiters" | "Liters"
<calories> ::= <amount>
<meal_type> ::= "breakfast" | "lunch" | "dinner" | "snack"

<date> ::= <year> " " <month> " " <day>
<year> ::= <digit> <digit> <digit> <digit>
<month> ::= "01" | "02" | "03" | "04" | "05" | "06"
           | "07" | "08" | "09" | "10" | "11" | "12"
<day> ::= "01" | "02" | "03" | "04" | "05" | "06" | "07" | "08" | "09"
        | "10" | "11" | "12" | "13" | "14" | "15" | "16" | "17" | "18" | "19"
        | "20" | "21" | "22" | "23" | "24" | "25" | "26" | "27" | "28"
        | "29" | "30" | "31"

<goal_option> ::= <set> | <show> | <set_composite_goal>
<set> ::=  "set" <date> <amount>
<show> ::= "show" <date>
<set_composite_goal> ::= "compose" <goal_list>
<goal_list> ::= <single_goal> "," (<goal_list> | <single_goal>)
<single_goal> ::= <date> <amount>

<digit> ::= [0-9]

<display> ::= "display" <date>


<dump_examples> ::= "dump examples"
```

## Examples
```Haskell
    Remove (Food "lettuce") 1 Kilograms 100 Lunch,
    Add (Food "tea") 734 Milliliters 50 Dinner,
    Meal (NestedAdd [SingleAdd (Food "tomato") 38 Grams 85 Dinner, SingleAdd (Food "tea") 7342 Milliliters 5 Dinner]),
    Goal (MultiGoal[Set (Date { year = 2023, month = 3, day = 15 }) 2000, Set (Date 2023 3 16) 1500]),
    Display (Date { year = 2023, month = 3, day = 15 }),
    Total (Date{year = 2010,month = 09,day = 09})
```