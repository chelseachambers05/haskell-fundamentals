import Prelude hiding (last, take, drop, replicate, zip, unzip, elem, init, (!!), concat, reverse, splitAt, sum, length, head, tail)

-- last: returns the last element of a list
last :: [a] -> a
last [x] = x
last (x:xs) = last xs

-- take: returns the first n elements of the list
take :: Int -> [a] -> [a]
take 0 xs = []
take n (x:xs) = x:take (n-1) xs

-- drop: throws away the first n elements and gives back the rest of the list
drop :: Int -> [a] -> [a]
drop 0 xs = xs
drop n (x:xs) = drop(n-1) xs

-- replicate: makes a list with of n copies of the item
replicate :: Int -> a -> [a]
replicate 0 x = []
replicate n x = x:replicate(n-1) x

-- zip: pairs up elements from two lists into a list of tuples
zip :: [a] -> [b] -> [(a,b)]
zip [] xs = []
zip xs [] = []
zip (x:xs) (y:ys) = (x,y): zip xs ys

-- unzip: takes a list of pairs and splits it into a tuple of two lists
unzip :: [(a,b)] -> ([a],[b])
unzip xs = ([x | (x,y) <- xs ],[y | (x,y) <- xs])

-- elem: checks whether x is somewhere in the list
elem :: Eq a => a -> [a] -> Bool
elem y [] = False
elem y (x:xs) = (x == y) || elem y xs

-- init: gives back every element except the last one
init :: [a] -> [a]
init [x] = []
init (x:xs) = x: (init xs)

-- !!: gives back the element at index n, counting from 0
(!!) :: [a] -> Int -> a
(!!) (x:xs) 0 = x
(!!) (x:xs) n = (!!) xs (n-1)

-- concat: takes a list of lists and glues them all together into one list
concat :: [[a]] -> [a]
concat [] = []
concat (x:xs) = x ++ concat xs

-- reverse: gives back the list in the opposite order
reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]

-- splitAt: splits the list into a tuple: the first n elements, and then everything after that

splitAt :: Int -> [a] -> ([a],[a])
splitAt n xs = (take n xs, drop n xs)

-- sum: adds up all the numbers in a list
sum :: [Int] -> Int
sum [] = 0
sum (x:xs) = x + sum xs

-- length: ounts how many elements are in the list
length :: [a] -> Int
length [] = 0
length (x:xs) = 1 + length xs

-- head: gives back the first element
head :: [a] -> a
head (x:xs) = x

-- tail: gives back everything except the first element
tail :: [a] -> [a]
tail (x:xs) = xs














