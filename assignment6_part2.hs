-- Chelsea Chambers
-- CSC 421 Assignment 6 Part 2
-- Higher Order Functions
import Prelude hiding (map, foldr,filter)

-- Part 2 Higher Order Functions
--    1. Give definitions of functions using map, filter and folder to take a list of integers, ns, and

--         a. return the list consisting of the squares of the integers in ns;
map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (n:ns) = f n: map f ns

square :: Int -> Int
square n = n * n

squareAll :: [Int] -> [Int]
squareAll ns = map square ns

--         b. return the sum of squares of items in ns;
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f s [] = s
foldr f s (n:ns) = f n(foldr f s ns)

sumSquares :: [Int] -> Int
sumSquares ns = foldr (+) 0 (squareAll ns)

--         c. check whether all items of the list are greater than zero
filter :: (a -> Bool) -> [a] -> [a]
filter p [] = []
filter p (n:ns)
    | p n = n: filter p ns
    | otherwise = filter p ns

greaterThan0 :: Int -> Bool
greaterThan0 n = n > 0

allGreater :: [Int] -> Bool
allGreater ns = filter greaterThan0 ns == ns
    
--    2. Define the sum of the squares of the natural numbers 1 to n using map and foldr.
sumSquares2 :: Int -> Int
sumSquares2 n = foldr (+) 0 (map square [1..n])

--    3.Define functions:

--       split :: [a] -> ([a],[a])
--       merge :: ([a],[a])->[a]
--      so that split will split a list into two lists, picking elements alternately, while merge will interleave the two lists.
split :: [a] -> ([a],[a])
split [] = ([], [])
split [x] = ([x], [])
split (x:y:xs) = (x:l, y:r)
    where (l, r) = split xs

merge :: ([a],[a]) -> [a]
merge ([], ys) = ys
merge (xs, []) = xs
merge (x:xs, y:ys) = x : y : merge (xs, ys)

--    4. Using a lambda abstration, the Boolean function not and the build-in function elem describe a function of type

--        Char -> Bool

--       which is True only on non-whitespace characters, that is those which are not elements of the list "\t\n".

nonWhitespace :: Char -> Bool
nonWhitespace = \ch -> not (elem ch " \t\n")

--    5. Define a function total

--        total :: (Integer -> Integer) -> (Integer -> Integer)

--       so that total f is the function which at value n gives the total

--       f 0 + f 1 + ... + f n
total :: (Integer -> Integer) -> (Integer -> Integer)
total f = \n -> foldr (+) 0 (map f [0..n])