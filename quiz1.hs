module Notes where
import Prelude hiding (max, fst, snd, elem)

-- exOr: outputs true when one but not both inputs are true (same as /=)
exOr :: Bool -> Bool -> Bool
exOr x y = (x || y) && not (x && y)

-- threeEqual: outputs true when all three inputs are equal
threeEqual :: Integer -> Integer -> Integer -> Bool
threeEqual m n p = (m == n) && (n == p)

-- max: return the maximum value of two integers
max :: Integer -> Integer -> Integer
max x y
    | x >= y = x
    | otherwise = y

{-
-- Convert char to int
fromEnum :: Char -> Int

-- Convert int to char
toEnum :: Int -> Char
-}

-- toUpper (using helper functions): convert a small letter to a capital letter
isSmall :: Char -> Bool
isSmall c = ('a' <= c) && (c <= 'z')

offset :: Int
offset = fromEnum 'A' - fromEnum 'a' 

toUpper :: Char -> Char
toUpper c 
    | isSmall c = toEnum(fromEnum c + offset)
    | otherwise = c

-- toUpper (self contained)
toUpper2 :: Char -> Char
toUpper2 c
    | ('a' <= c) && (c <= 'z') = toEnum(fromEnum c +(fromEnum 'A' - fromEnum 'a'))
    | otherwise = c

-- isDigit: checks whether a character is a digit
isDigit :: Char -> Bool
isDigit ch = ('0' <= ch) && (ch <= '9')

{-
-- convert integer to float
fromInteger :: Integer -> Float
fromIntegral :: Int -> Float -- int or any integral value
-}

-- max: returns the max of three integers
maxThree :: Integer -> Integer -> Integer -> Integer
maxThree x y z
    | (x >= y) && (x >= z) = x
    | (y >= x) && (y >= z) = y
    | otherwise = z

-- maxThree: returns the max of three integers. Uses previously defined max func
maxThree2 :: Integer -> Integer -> Integer -> Integer
maxThree2 x y z = max (max x y) z

-- middleNumber: returns the middle number of three integers
middleNumber :: Integer ->  Integer ->  Integer -> Integer
middleNumber x y z
    | between y x z = x
    | between x y z = y
    | otherwise = z

between :: Integer ->  Integer ->  Integer -> Bool
between x y z
    | y >= x && y <= z = True
    | y <= x && y >= z = True
    | otherwise        = False

-- triArea: calculates the area of a triangle. Must satisfy rule ( the length of the side is less than the sum of the other two sides)
triArea :: Float -> Float -> Float -> Float
triArea a b c
    | possible = sqrt(s*(s-a)*(s-b)*(s-c))
    | otherwise = 0
    where
        s = (a + b + c)/2
        possible = a < (b + c) && b < (a + c) && c < (a + b)

-- isOdd, isEven
isOdd, isEven :: Integer -> Bool

isOdd n
    | n <= 0 = False
    | otherwise = isEven(n-1)

isEven n
    | n < 0 = False
    | n == 0 = True
    | otherwise = isOdd(n-1)

-- Rock, paper, scissors
data Move = Rock | Paper | Scissors deriving (Show, Eq)

beat :: Move -> Move
beat Rock = Paper
beat Paper = Scissors
beat Scissors = Rock

lose :: Move -> Move
lose Rock = Scissors
lose Paper = Rock
lose _ = Paper

-- factorial
fac :: Integer -> Integer
fac n
    | n == 0 = 1
    | n > 0 = fac(n - 1) * n

-- power2: returns powers of two for natural numbers
power2:: Integer -> Integer
power2 n
    | n == 0 = 1
    | n > 0 = power2(n-1) * 2

-- minAndMax:  return both the minimum and maximum of two integers
minAndMax :: Integer -> Integer -> (Integer, Integer)
minAndMax x y
    | x >= y = (y, x)
    | otherwise = (x, y)

-- pattern matching
addPair :: (Integer, Integer) -> Integer 
addPair(x,y) = x+y 

shift :: ((Integer,Integer), Integer) -> (Integer, (Integer, Integer))
shift ((x,y),z) = (x,(y,z))

-- selector functions
type ShopItem = (String, Int)
name :: ShopItem -> String
price :: ShopItem -> Int
name (n,p) = n
price (n,p) = p 

-- built in selector functions
fst (x,y) = x
snd (x,y) = y 

-- fibonacci numbers
fibStep :: (Integer, Integer) -> (Integer, Integer)
fibStep (u, v) = (v, (u + v))

fibPair :: Integer -> (Integer, Integer)
fibPair n      
    | n == 0 = (0 , 1)
    | otherwise = fibStep (fibPair (n - 1))
    
fastFib :: Integer -> Integer
fastFib = fst . fibPair

-- data types
data Shape = Circle Float |
            Rectangle Float Float
            deriving (Eq, Ord, Show)

isRound :: Shape -> Bool
isRound (Circle _) = True
isRound (Rectangle _ _) = False

area :: Shape -> Float
area (Circle r) = pi*r*r
area (Rectangle h w) = h*w

-- convert a value to string
example1 :: String
example1 = show (2 + 3)

example2 :: String
example2 = show (True || False)

-- convert a string to the value it represents
example3 :: Bool
example3 = read "True"

example4 :: Integer
example4 = read "3"

-- list comprehension
isEven2 :: Integer -> Bool 
isEven2 n = (n `mod` 2 == 0) 
ex :: [Integer]
ex = [1,2,3,4,5,6]

evenResults :: [Bool]
evenResults = [isEven2 n | n <- ex]

addPairs :: [(Integer, Integer)] -> [Integer]
addPairs pairList = [ m+n | (m,n) <- pairList]

digits :: String -> String
digits st = [ ch | ch<-st , isDigit ch ]

allEven xs = (xs == [x | x<-xs, isEven2 x])
allOdd  xs = ([] == [x | x<-xs, isEven2 x])

totalRadii :: [Shape] -> Float
totalRadii shapes = sum [r | Circle r <- shapes]

{-
-- polymorphic length function
length :: [a] -> Int

-- zip & unzip: convert between pairs of lists and lists of pairs
zip :: [a] -> [b] -> [(a,b)]
unzip :: [(a,b)] -> ([a],[b])
-}

-- recursive sum function
mySum :: [Integer] -> Integer
mySum [] = 0
mySum (x:xs) = x + mySum xs 

-- myElem: Check whether an Integer is an element of an Integer list
myElem :: Integer -> [Integer] -> Bool
myElem x [] = False
myElem x (y:ys) = (x==y) || (myElem x ys)