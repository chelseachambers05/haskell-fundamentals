-- Chelsea Chambers
-- CSC 421 Assignment 6 Part 1
-- Text Processing

import Prelude hiding (Word, getLine)

-- Part 1 Text Processing
-- We have studied text processing example in class. In this assignment, you are asked to modify or add functions to the text processing program we coded in the class.

--    1. Implement the text processing example. 

whitespace :: String
whitespace = ['\n','\t',' ']
 
type Word = String
type Line = [Word]
type Doc = String
 
getWord :: String -> String
getWord [] = []
getWord (x:xs)
    | elem x whitespace = []
    | otherwise = x : getWord xs
 
dropWord :: String -> String
dropWord [] = []
dropWord (x:xs)
    | elem x whitespace = (x:xs)
    | otherwise = dropWord xs
 
dropSpace :: String -> String
dropSpace [] = []
dropSpace (x:xs)
    | elem x whitespace = dropSpace xs
    | otherwise = (x:xs)
 
splitWords :: String -> [Word]
splitWords st = split (dropSpace st)
 
split :: String -> [Word]
split [] = []
split st = (getWord st) : split (dropSpace (dropWord st))
 
lineLen :: Int
lineLen = 30
 
getLine :: Int -> [Word] -> Line
getLine len [] = []
getLine len (w:ws)
    | length w <= len = w : restOfLine
    | otherwise = []
    where
        newlen = len - (length w + 1)
        restOfLine = getLine newlen ws
 
dropLine :: Int -> [Word] -> [Word]
dropLine len [] = []
dropLine len (w:ws)
    | length w <= len = restOfLine
    | otherwise = (w:ws)
    where
        newlen = len - (length w + 1)
        restOfLine = dropLine newlen ws
 
splitLines :: [Word] -> [Line]
splitLines [] = []
splitLines ws = getLine lineLen ws : splitLines (dropLine lineLen ws)

fill :: String -> [Line]
fill = splitLines.splitWords

joinLine :: Line -> String
joinLine [] = []
joinLine (w:ws) = w ++ " " ++ (joinLine ws)

joinLines :: [Line] -> String
joinLines [] = []
joinLines (l:ls) = (joinLine l) ++ "\n" ++ (joinLines ls)
--    2. Define the formatLines function using map function. The type of the formatLines function is defined as:
--         formatLines :: (Int -> Line -> String) -> Int -> [Line] -> [String]
formatLines :: (Int -> Line -> String) -> Int -> [Line] -> [String]
formatLines f n ls = map (f n) ls

combine :: [String] -> String
combine [] = []
combine (s:ss) = s ++ "\n" ++ combine ss

formatDoc :: (Int -> Line -> String) -> Doc -> Doc
formatDoc f = combine.formatLines f lineLen.splitLines.splitWords

--    3. Define a function that will format the text to left alignment.
--         leftAlign :: Int -> Line -> String
lineStr :: Line -> String
lineStr [] = []
lineStr [w] = w
lineStr (w:ws) = w ++ " " ++ lineStr ws

leftAlign :: Int -> Line -> String
leftAlign n l = st ++ replicate (n - length st) ' '
    where
        st = lineStr l
--    4. Define a function that will format the text to center alignment.
--         centerAlign :: Int -> Line -> String
centerAlign :: Int -> Line -> String
centerAlign n l = replicate front ' ' ++ st ++ replicate back ' '
    where
        st = lineStr l
        extra = n - length st
        front = extra `div` 2
        back = extra - front
 
--    5. Design a function
--         wc::String -> (Int,Int,Int)
--       which when called with a string, will return the number of words, characters, and lines in the string. The end of a line in the string is signalled by the newline character '\n'.
wc :: String -> (Int,Int,Int)
wc st = (numWords, numChars, numLines)
    where
        numWords = length (splitWords st)
        numChars = length st
        numLines = length (filter (\c -> c == '\n') st)