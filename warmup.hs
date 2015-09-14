-- Uppvärmning
-- Progp15
-- Peter Jonsson och Lucas Ljungberg
-- 2015-09-01
--

-- Required
module F1 where

    import Data.Char -- Import needed for isAlpha function
    import Data.Bits -- Import needed for bitwise operations

    -- Uppgift: Fibonacci

    -- Fibonacci funktion
    -- Returns the n-th Fibonacci number 
    fib :: Int -> Integer
    fib n = let list = 0 : 1 : zipWith (+) list (tail list) in list !! n

    -- Uppgift: Rövarspråket

    -- Checks if letter is vowel
    isVowel :: Char -> Bool
    isVowel x = elem x "aeyuio"

    -- Converts a string to rövarspråk
    rovarsprak :: String -> String
    rovarsprak "" = ""
    rovarsprak s = if isVowel(head s)
        then (head s) : rovarsprak(tail s)
        else 
            (head s) : 'o': (head s) : rovarsprak(tail s)


    -- Reverses Rövarspråket
    karpsravor :: String -> String
    karpsravor "" = ""
    karpsravor s = if isVowel(head s)
        then (head s) : karpsravor(tail s)
        else
            (head s) : karpsravor(tail (drop 2 s))


    -- Uppgift: Medellängd

    medellangd :: String -> Double
    medellangd "" = 0
    medellangd s = (fromIntegral numChars) / (fromIntegral numWords)
        where
            wordList = words [if isAlpha x || x == ' ' then x else ' ' | x <- s]
            numChars = sum (map length(wordList))
            numWords = length wordList





    -- Uppgift: Listskyffling

    -- Shuffles an array according to a rule
    -- The rule is to always take every other element to the end of the "new" list.
    skyffla :: [a] -> [a]
    skyffla [] = []
    skyffla x = (forNth 2 x) ++ (skyffla (forNth 2 t))
        where t = tail x

    -- Returns every other nth in the given list as a list
    forNth :: Int -> [a] -> [a]
    forNth n [] = []
    forNth n list = [list!!i | i <- [0, n..(length list) - 1]]
