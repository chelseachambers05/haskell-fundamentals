
-- 1. Using the addition function over the natural numbers, give a recursive definition of multiplication of natural numbers.
multiply :: Integer -> Integer -> Integer
multiply m n
    | n == 0 = 0
    | otherwise = m + multiply m (n-1)

{-2. Question 
    The integer square root of a positive integer n is the largest 
    integer whose square is less than or equal to n. For instance, 
    the integer square roots of 15 and 16 are 3 and 4, respectively. 
    Given a primitive recursive definition of this function.
-}
sqrtR :: Integer -> Integer
sqrtR n
    | n == 0 = 0
    | (r + 1)*(r + 1) <= n = r + 1
    | otherwise = r
    where
        r = sqrtR (n - 1)

{-3. Question
    Give a recursive definition of a function to find the highest common factor of two positive integers.
-}
hcf :: Integer -> Integer -> Integer
hcf m n
    | n == 0 = m
    | otherwise = hcf n (mod m n)


{-4. Question 
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
    | otherwise = (z, y, x)


{-5.  Question
    Define a function to give the length of the perimeter of a geometrical shape, of type Shape. 
    What is the type of this function?
-}

{-6. Question 
    Add an extra constructor to Shape for triangles, and extend the functions isRound, 
    area and perimeter to include triangles.
-}

data Shape = Circle Float |
             Rectangle Float Float |
             Triangle Float Float Float
             deriving (Eq, Ord, Show)

perimeter :: Shape -> Float
perimeter (Circle r) = 2 * pi * r
perimeter (Rectangle h w) = 2 * (h + w)
perimeter (Triangle a b c) = a + b + c

{- 7.  Question 
    Give a definition of a function below,
	 which triples all the elements of a list of integers.
-}

tripleAll :: [Integer] -> [Integer]
tripleAll xs = [3*x | x <- xs]

{-8. Question 
    Give a definition of a function
	which converts all small letters in a String into capitals, 
    leaving the other characters unchanged. 
-}

isSmall :: Char -> Bool
isSmall c = ('a' <= c) && (c <= 'z')

offset :: Int
offset = fromEnum 'A' - fromEnum 'a' 

toUpper :: Char -> Char
toUpper c 
    | isSmall c = toEnum(fromEnum c + offset)
    | otherwise = c

capitalize :: String -> String
capitalize ch = [toUpper c | c <- ch]


{-9. Question 
    Define the function below,
	which returns the list of divisors of a positive integer (and the empty list for other inputs). 
    For instance,
	divisors 12 = [1,2,3,4,6,12]
-} 

divisor :: Integer -> [Integer]
divisor n = [d | d <- [1..n], mod n d == 0]


{-
10.  Question
    A prime number $n$ is a number whose only divisors are 1 and n. 
    Using divisors or otherwise define a function
	which checks whether or not a positive integer is prime (and returns False if its input is not a positive integer).
-}
isPrime :: Integer -> Bool
isPrime n
    | n <= 1 = False
    | otherwise = divisor n == [1, n]