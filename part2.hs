-- Chelsea Chambers
-- # Part 2 Supermarket Bill

{- 1. Please implement the supermarket bill program that we've covered in the class. Use the example database from the slides. 

Please add the following functions to the program:
### 2. Discount for Multiple Buys
Modify the billing system to apply a discount for every two bottles of sherry bought, reducing the price by 1.00 per pair. Given the example bar code list:
``` 1234, 4719, 3814, 1112, 1113, 1234 ```
the bill should reflect the discount. Implement the following helper functions:

makeDiscount :: BillType -> Int
formatDiscount :: Int -> String

and modify:

formatBill :: BillType -> String
-}

type Name = String
type Price = Int
type BarCode = Int

type Database = [(BarCode, Name, Price)]

codeIndex :: Database
codeIndex = [ (4719, "Fish Fingers", 121),
              (5643, "Nappies", 1010),
              (3814, "Orange Jelly", 56),
              (1111, "Hula Hoops", 21),
              (1112, "Hula Hoops (Giant)", 133),
              (1234, "Dry Sherry, 1lt", 540)]

type TillType = [BarCode]
type BillType = [(Name, Price)]

lineLength :: Int
lineLength = 30

produceBill :: TillType -> String
produceBill = formatBill . makeBill

look :: Database -> BarCode -> (Name, Price)
look db bCode
    | [] == find = ("Unknown Item", 0)
    | otherwise = (name (head find), price (head find))
    where
        find = [(b,n,p) | (b,n,p) <- db, b == bCode]
        name (b,n,p) = n
        price (b,n,p) = p

lookUp :: BarCode -> (Name, Price)
lookUp bCode = look codeIndex bCode

makeBill :: TillType -> BillType
makeBill bx = [ lookUp bCode | bCode <- bx ]

formatCent :: Price -> String
formatCent p
    | f < 10 = (show i) ++ ".0" ++ (show f)
    | otherwise = (show i) ++ "." ++ (show f)
    where
        i = p `div` 100
        f = p `mod` 100

formatLine :: (Name, Price) -> String
formatLine (n, cent) = n ++ sps ++ dollar ++ "\n"
    where
        dollar = formatCent cent
        nOfsps = lineLength - (length n) - (length dollar)
        sps = replicate nOfsps '.'

formatLines :: [(Name, Price)] -> String
formatLines [] = ""
formatLines (x:xs) = formatLine x ++ formatLines xs

makeTotal :: BillType -> Price
makeTotal bx
    | [] == bx = 0
    | otherwise = snd x + makeTotal xs
    where (x:xs) = bx

formatTotal :: Price -> String
formatTotal p = "\n" ++ t ++ sps ++ dollar
    where
        t = "Total"
        dollar = formatCent p
        nOfsps = lineLength - (length t) - (length dollar)
        sps = replicate nOfsps '.'

makeDiscount :: BillType -> Int
makeDiscount bx = ((length find) `div` 2) * 100
    where find = [(n,p) | (n,p) <- bx, n == "Dry Sherry, 1lt"]

formatDiscount :: Int -> String
formatDiscount 0 = ""
formatDiscount d = t ++ sps ++ dollar ++ "\n"
    where
        t = "Discount"
        dollar = formatCent d
        nOfsps = lineLength - (length t) - (length dollar)
        sps = replicate nOfsps '.'

formatBill :: BillType -> String
formatBill bx = "        Haskell Stores\n\n" ++ formatLines bx ++ formatDiscount (makeDiscount bx) ++ formatTotal (makeTotal bx - makeDiscount bx)

{-  3. Updating the Database
Design functions to update the database of bar codes. Specifically, implement a function to add a `(BarCode, (Name, Price))` pair to the database while ensuring any previous reference to the same barcode is removed.
-}
removeBarCode :: Database -> BarCode -> Database
removeBarCode db bCode = [(b,n,p) | (b,n,p) <- db, b /= bCode]

addBarCode :: Database -> (BarCode, (Name, Price)) -> Database
addBarCode db (bCode, (n, p)) = [(bCode, n, p)] ++ removeBarCode db bCode