import Prelude hiding (last, init, tail, replicate, take, drop, splitAt, zip, unzip, reverse)
-- Chelsea Chambers
-- # Part 1: Haskell Code

-- Implement the following build-in functions:
{-1. 
    isEmpty :: [a] -> Bool
 
    This function should check whether a list is empty. The effect of:

    isEmpty [1,2,3]

    should be `False`.      
-}
isEmpty :: [a] -> Bool
isEmpty [] = True
isEmpty _ = False

{-2. 

    last :: [a] -> a

    This function should return the last element of a list. The effect of:

    last [1,2,3]

    should be `3`.
-}
last :: [a] -> a
last [x] = x
last (x:xs) = last xs 


{-3. 

    init :: [a] -> [a]

    This function should return all elements of a list except the last one. The effect of:

    init [1,2,3]

    should be `[1,2]`.
-} 
init :: [a] -> [a]
init [x] = []
init (x:xs) = x:(init xs)

{-4. 

    tail :: [a] -> [a]

    This function should return all elements of a list except the first one. The effect of:

    tail [1,2,3]

    should be `[2,3]`.
-}
tail :: [a] -> [a]
tail (x:xs) = xs 

{-5. 

    replicate :: Int -> a -> [a]

    This function should return a list of a given length, filled with a given element. The effect of:

    replicate 3 'a'

    should be `['a', 'a', 'a']`.
-}
replicate :: Int -> a -> [a]
replicate 0 x = []
replicate n x = x: replicate(n-1) x

{-6. 

    take :: Int -> [a] -> [a]

    This function should return the first n elements of a list. The effect of:

    take 2 [1,2,3,4,5]

    should be `[1,2]`.
-}
take :: Int -> [a] -> [a]
take 0 xs = []
take n (x:xs) = x: take(n-1) xs

{-7. 

    drop :: Int -> [a] -> [a]

    This function should return all elements of a list except the first n elements. The effect of:

    drop 2 [1,2,3,4,5]

    should be `[3,4,5]`.
-}
drop :: Int -> [a] -> [a]
drop 0 xs = xs
drop n (x:xs) = drop(n-1) xs

{-8. 

    splitAt :: Int -> [a] -> ([a], [a])

    This function should split a list into two parts at a given index. The effect of:

    splitAt 2 [1,2,3,4] should be ([1,2][3,4])
-}
splitAt :: Int -> [a] -> ([a],[a])
splitAt n xs = (take n xs, drop n xs)

{-9. 

    zip :: [a] -> [b] -> [(a, b)]

    This function should combine two lists into a list of pairs. The effect of:

    zip [1,2,3] ['a','b','c']

    should be `[(1,'a'),(2,'b'),(3,'c')]`.
-}
zip :: [a] -> [b] -> [(a,b)]
zip xs [] = []
zip [] xs = []
zip (x:xs) (y:ys) = (x,y): zip xs ys

{-10. 

    unzip :: [(a, b)] -> ([a], [b])

    This function should split a list of pairs into two lists. The effect of:

    unzip [(1,'a'),(2,'b'),(3,'c')]

    should be `([1,2,3],['a','b','c'])`.
-}
unzip :: [(a,b)] -> ([a],[b])
unzip xs = ([x | (x,y) <- xs],[y | (x,y) <- xs])


{- 2. Implement the following functions:
1. Fibonacci Table
Define a function:

fibTable :: Int -> String

This function should generate a table of Fibonacci numbers. The effect of:

putStr (fibTable 6)

should produce:

n   fib n
0   0
1   1
2   1
3   2
4   3
5   5
6   8
-}
fib :: Int -> Int
fib n
    | n == 0 = 0
    | n == 1 = 1
    | otherwise = fib(n-1) + fib(n-2)

fibLine :: Int -> String
fibLine n = show n ++ "   " ++ show (fib n) ++ "\n"

fibLines :: [Int] -> String
fibLines [] = ""
fibLines (x:xs) = fibLine x ++ fibLines xs

fibTable :: Int -> String
fibTable n = "n   fib n\n" ++ fibLines [0..n]

{- 2. Palindrome Checker
Define a function:

isPalin :: String -> Bool

This function should check whether a given string is a palindrome. The function should ignore punctuation, whitespace, and case differences. For example, the string:
``` Madam I’m Adam ```
should be identified as a palindrome.-}
isLetter :: Char -> Bool
isLetter ch = (('a' <= ch) && (ch <= 'z')) || (('A' <= ch) && (ch <= 'Z'))

offset :: Int
offset = fromEnum 'a' - fromEnum 'A'

toLowerCase :: Char -> Char
toLowerCase ch
    | ('A' <= ch) && (ch <= 'Z') = toEnum (fromEnum ch + offset)
    | otherwise = ch
    
letters :: String -> String
letters st = [ toLowerCase ch | ch<-st , isLetter ch ]

reverse :: String -> String
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]

isPalin :: String -> Bool
isPalin s = letters s == reverse (letters s)
    