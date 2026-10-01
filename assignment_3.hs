{- 1. Explain the effect of the function defined here:
	mystery :: Integer -> Integer -> Integer -> Bool
	mystery m n p = not ((m==n) && (n==p))

    Answer: The function mystery returns True when at least one integer is different
-}

{-2.  Define a function 

	threeDifferent :: Integer -> Integer -> Integer -> Bool

	so that the result of `threeDifferent m n p` is True only if all three of the number m, n and p are different.
-}
threeDifferent :: Integer -> Integer -> Integer -> Bool
threeDifferent m n p = ((m/=n) && (n/=p) && (m/=p))

{-3.  This question is about the function:

	fourEqual :: Integer ->  Integer -> Integer -> Integer -> Bool

	which returns the value True only if all four of its arguments are equal. Give a definition of `fourEqual` which uses the function `threeDifferent` in its definition. 
-}
fourEqual :: Integer ->  Integer -> Integer -> Integer -> Bool
fourEqual m n o p
    | threeDifferent m n p == True = False
    | otherwise = ((m==n) && (n==p) && (n==o))

-- 4.  Define a function to convert small letters to capitals which returns unchanged characters which are not small letters.

offset :: Int
offset = fromEnum 'A' - fromEnum 'a'

isSmall :: Char -> Bool
isSmall ch = (ch >= 'a') && (ch <= 'z')

toUpper :: Char -> Char
toUpper ch
    | isSmall ch = toEnum (fromEnum ch + offset)
    | otherwise = ch

{-
5. Define the function 

	charToNum :: Char -> Int

	which converts a digit like '8' to its value, 8. The value of non-digits should be taken to be 0.
-} 
isDigit :: Char -> Bool
isDigit d = (d >= '0') && (d <= '9')

charToNum :: Char -> Int
charToNum ch
    | isDigit ch = fromEnum ch - fromEnum '0'
    | otherwise = 0

{- 6. Define a function

	onThreeLines :: String -> String -> String -> String

	which takes three strings and returns a single string which when printed; shows the three strings on separate lines.
-}
onThreeLines :: String -> String -> String -> String
onThreeLines s1 s2 s3 = s1 ++ "\n" ++ s2 ++ "\n" ++ s3

{-7. Define a function

	romanDigit :: Char -> String

	which converts a digit to its representation in Roman numerals, so at '7' it will have the value "VII" and so on.
-}
romanDigit :: Char -> String
romanDigit ch = case ch of
    '0' -> ""
    '1' -> "I"
    '2' -> "II"
    '3' -> "III"
    '4' -> "IV"
    '5' -> "V"
    '6' -> "VI"
    '7' -> "VII"
    '8' -> "VIII"
    '9' -> "IX"
    _   -> ""

-- 8. Give a function to return the average of three integers

averageThree :: Integer -> Integer -> Integer -> Float 
averageThree m n p = fromInteger(m + n + p) / 3


{- 9.  Using the above definition of `averageThree` to define a function:

	howManyAboveAverage :: Integer -> Integer -> Integer -> Float

	which returns how many of its inputs are larger than their average value.
-}
howManyAboveAverage :: Integer -> Integer -> Integer -> Float
howManyAboveAverage m n p = c1 + c2 + c3
    where
        avg = averageThree m n p
        c1 = if fromInteger m > avg then 1 else 0
        c2 = if fromInteger n > avg then 1 else 0
        c3 = if fromInteger p > avg then 1 else 0

{-10. Write a function
	
	numberNDroots :: FLoat -> Float -> Float -> Integer

	that given the coefficients of the quadratic, a b and c, will return how many roots the equation has. You may assume that the equation is non-degenerate. 
-}
numberOfRoots :: Float -> Float -> Float -> Integer
numberOfRoots a b c
    | discriminant > 0  = 2
    | discriminant == 0 = 1
    | otherwise          = 0
    where
        discriminant = b*b - 4*a*c

-- 11. Using the addition function over the natural numbers, give a recursive definition of multiplication of natural numbers.
multiply :: Integer -> Integer -> Integer
multiply m n
    | n == 0 = 0
    | n > 0  = m + multiply m (n-1)

{-12. Question 
    The integer square root of a positive integer n is the largest 
    integer whose square is less than or equal to n. For instance, 
    the integer square roots of 15 and 16 are 3 and 4, respectively. 
    Given a primitive recursive definition of this function.
-}
sqrtR :: Integer -> Integer
sqrtR n
    | n == 0 = 0
    | n > 0  = if (r+1)*(r+1) <= n then r+1 else r
    where
        r = sqrtR (n-1)

{-13. Question
    Give a recursive definition of a function to find the highest common factor of two positive integers.
-}
hcf :: Integer -> Integer -> Integer
hcf a b
    | b == 0 = a
    | otherwise = hcf b (mod a b)

{-
14. Question 
    Give a definition of the function below

    orderTriple :: (Integer, Integer, Integer) -> (Integer, Integer, Integer)

	 which puts the elements of a triple of three integers into ascending order. 
-}
orderTriple :: (Integer, Integer, Integer) -> (Integer, Integer, Integer)
orderTriple (x, y, z)
    | x <= y && y <= z = (x, y, z)
    | x <= z && z <= y = (x, z, y)
    | y <= x && x <= z = (y, x, z)
    | y <= z && z <= x = (y, z, x)
    | z <= x && x <= y = (z, x, y)
    | otherwise        = (z, y, x)


{- 15.  Question
    Define a function to give the length of the perimeter of a geometrical shape, of type Shape. 
    What is the type of this function?

    Answer: The type of the funcion is Shape -> Float
-}
data Shape = Circle Float |
             Rectangle Float Float
             deriving (Eq, Ord, Show)

perimeter :: Shape -> Float
perimeter (Circle r) = 2 * pi * r
perimeter (Rectangle h w) = 2 * (h + w)

{- 16. Question 
    Add an extra constructor to Shape for triangles, and extend the functions isRound, 
    area and perimeter to include triangles.
-}
data Shape2 = Circle2 Float |
             Rectangle2 Float Float |
             Triangle Float Float Float
             deriving (Eq, Ord, Show)

isRound :: Shape2 -> Bool
isRound (Circle2 _) = True
isRound (Rectangle2 _ _) = False
isRound (Triangle _ _ _) = False

area :: Shape2 -> Float
area (Circle2 r) = pi*r*r
area (Rectangle2 h w) = h*w
area (Triangle a b c)
    | possible = sqrt(s*(s-a)*(s-b)*(s-c))
    | otherwise = 0
    where
        s = (a + b + c)/2
        possible = a < (b + c) && b < (a + c) && c < (a + b)

perimeter2 :: Shape2 -> Float
perimeter2 (Circle2 r) = 2 * pi * r
perimeter2 (Rectangle2 h w) = 2 * (h + w)
perimeter2 (Triangle a b c) = a + b + c