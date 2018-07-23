import Data.List
import Data.Maybe

alpha = ['a'..'z']

-- Converts lower-case letter in the range 'a' to 'z' into corresponding natural
-- number in the range 0 to 25
let2nat :: Char -> Int
let2nat x = fromJust $ elemIndex x alpha

-- Inverse function to let2nat
nat2let :: Int -> Char
nat2let x = alpha !! x

-- Applies a shift factor in the range 0 to 25 to a lower-case letter in the 
-- range 'a' to 'z', wrapping around at the end of the alphabet
shift :: Int -> Char -> Char
shift x y 
    | elem y alpha = nat2let (((let2nat y) + x) `mod` 26)
    | notElem y alpha = y

-- Applies a reversed shift factor in the range 0 to 25 to a lower-case letter in the 
-- range 'a' to 'z', wrapping around at the end of the alphabet
shift' :: Int -> Char -> Char
shift' x y 
    | elem y alpha = nat2let (((let2nat y) - x) `mod` 26)
    | notElem y alpha = y

-- Encodes a string using a given shift factor
encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

-- Inverse function to encode
decode :: Int -> String -> String
decode n xs = [shift' n x | x <- xs]

-- Approximate frequencies of the 26 letters of the alphabet
table :: [Float]
table = [8.2, 1.5, 2.8, 4.3, 12.7, 2.2, 2.0, 6.1, 7.0, 
         0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0, 
         6.3, 9.1, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

-- Calculates the number of lower-case letter in a string         
lowers :: String -> Int
lowers [] = 0
lowers (x:xs)
    | elem x alpha = lowers xs + 1
    | otherwise = lowers xs

-- Calculates the number of a given character in a string
count :: Char -> String -> Int
count n [] = 0
count n (x:xs)
    | x == n = count n xs + 1
    | otherwise = count n xs

-- Calculates the percentage of one integer with respect to another
percent :: Int -> Int -> Float
percent x y = (fromIntegral x) / (fromIntegral y) * 100

-- Returns the list of percentage frequencies of each of the lower-case letters
freqs :: String -> [Float]
freqs xs = [percent (count x xs) (lowers xs) | x <- alpha]

-- Calculates chi square statistic for a list of observed frequencies os
-- with respect to a list of expected frequencies es
chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o - e)^2) / e | (o, e) <- zip os es]

-- Returns first position at which a value occurs in a list
position :: Eq a => a -> [a] -> Int
position x xs = fromJust $elemIndex x xs

-- Rotates a list n places to the left, wrapping around the start of the list,
-- and assuming n is in the range 0 - length of list
rotate :: Eq a => Int -> [a] -> [a]
rotate n xs = [ xs !! ((y + n) `mod` length xs) | (x, y) <- zip xs [0..length xs]]

-- Attempts to decode a string
-- 1. calculate letter frequencies in string
-- 2. calculate chi square value of each rotation
-- 3. Use position of minimum value in list as shift factor
-- 4. Decode original string
crack :: String -> String
crack [] = []
crack xs = decode (position (minimum as) as) xs
    where
        as = makeList xs

makeList xs = [chisqr(rotate i (freqs xs)) table | i <- [0..length xs]]