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
- **Operations:**
    - `add`: Add a food item to a meal.
    - `remove`: Remove a food item from a meal.
    - `meal`: Define a recursive meal structure (can contain foods or other meals).     
    - `total`: Show total calories for a given date.
    - `display`: Display food intake of your date of choice.

## BNF

```bnf
<command> ::= <meal> | <add> | <remove> | <total_calories> | <display> | <dump_examples>

<meal> ::= "meal " <meal_body>
<meal_body> ::= <add> | <meal_body> ", " <meal_body>
<add> ::= "add " <data> " to " <meal_type>
<remove> ::= "remove " <data> " from " <meal_type>
<total_calories> ::= "total " <date> 

<data> ::= <food> ", " <amount> <unit> ", " <calories>
<food> ::= <string>
<amount> ::= <digit> | <digit> <amount>
<unit> ::= "Grams" | "Kilograms" | "Milliliters" | "Liters"
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
<digit> ::= [0-9]

<display> ::= "display " <date>

<dump_examples> ::= "dump examples"
```

## Examples
```
    Remove (Food "lettuce") 1 Kilograms 100 Lunch,
    Add (Food "tea") 734 Millliliters 50 Dinner,
    Meal (CombineAdd (SingleAdd (Food "tomato") 38 Grams 85 Dinner) (SingleAdd (Food "tea") 7342 Milliliters 5 Dinner)),
    Display (Date 2010 10 10),
    Total (Date 2010 10 10)
```