-- 1. add: sum of two numbers
add :: Integer -> Integer -> Integer
add a b = a + b

-- 2. double: double a number
double :: Integer -> Integer
double n = 2 * n

-- 3. area: area of a circle
area :: Float -> Float
area r = pi * r * r

-- 4. cube: cube of a number
cube :: Integer -> Integer
cube n = n * n * n

-- 5. doubleArea: double the area of a circle
doubleArea :: Float -> Float
doubleArea r = 2 * area r

-- 6. cylinder: volume of a cylinder
cylinder :: Float -> Float -> Float
cylinder r h = area r * h

-- 7. celsiusToFahrenheit: convert Celsius to Fahrenheit
celsiusToFahrenheit :: Float -> Float
celsiusToFahrenheit c = (c * 9 / 5) + 32

-- 8. exclusiveor: exclusive or of two boolean values
exclusiveor :: Bool -> Bool -> Bool 
exclusiveor x y = (x == True && y == False) || (x == False && y == True) 

-- 9. nand: not and of two boolean values
nAnd1 :: Bool -> Bool -> Bool 
nAnd1 x y = not (x == True && y == True) 

-- version 2
nAnd2 :: Bool -> Bool -> Bool 
nAnd2 x y
    | x == True && y == False = True 
    | x == False && y == True = True 
    | x == False && y == False = True 
    | otherwise = False

-- 10. min: return the smaller of two numbers
myMin :: Integer -> Integer -> Integer
myMin a b
  | a <= b    = a
  | otherwise = b