-- Problem: https://www.codewars.com/kata/525f3eda17c7cd9f9e000b39/train/haskell

-- This time we want to write calculations using functions and get the results. Let's have a look at some examples:

-- seven $ times $ five   ->  35 :: Int
-- four $ plus $ nine     ->  13 :: Int
-- eight $ minus $ three  ->   5 :: Int
-- six $ dividedBy $ two  ->   3 :: Int

-- Requirements:

--     There must be a function for each number from 0 ("zero") to 9 ("nine")
--     There must be a function for each of the following mathematical operations: plus, minus, times, dividedBy
--     Each calculation consist of exactly one operation and two numbers
--     The most outer function represents the left operand, the most inner function represents the right operand
--     Division should be integer division. For example, this should return 2, not 2.666666...:

-- eight $ dividedBy $ three


module CalculatingWithFunctions (plus,minus,times,dividedBy,zero,one,two,three,four,five,six,seven,eight,nine) where


plus,minus,times,dividedBy :: ((Int -> Int) -> Int) -> (Int -> Int)
plus x = \y -> y + (x id)
minus x = \y -> y - (x id)
times x = \y -> y * (x id)
dividedBy x = \y -> y `div` (x id)



zero,one,two,three,four,five,six,seven,eight,nine :: (Int -> Int) -> Int
zero f = f 0
one f = f 1
two f = f 2
three f = f 3
four f = f 4
five f = f 5
six f = f 6
seven f = f 7
eight f = f 8
nine f = f 9
