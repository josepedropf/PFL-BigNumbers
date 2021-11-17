{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# LANGUAGE MultiWayIf #-}
import Distribution.Simple.Build (build)
--type BigNumber = [Int]
--Big Decision

--type Sign = Bool
--type Digits = [Int]

data BigNumber = BigNumber {sign :: Bool, digits :: [Int]} deriving (Eq, Read)

--data Digit = 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9

nDigits :: Int -> Int
nDigits n
  | n < 0 = nDigits (abs n)
  | abs n >= 10 = 1 + nDigits (div n 10)
  | otherwise = 1


buildBigN :: Bool -> Int -> [Int] -> BigNumber
buildBigN b n l 
  | nDigits n == 1 = BigNumber b (n:l)
  | otherwise = buildBigN b (div (abs n) 10) (mod (abs n) 10 : l)

intToBigN :: Int -> BigNumber
intToBigN n
  | n < 0 = buildBigN False n []
  | otherwise = buildBigN True n []

instance Show BigNumber where 
  show (BigNumber b l)
    | b = "+" ++ show l
    | otherwise = "-" ++ show l

getBigNSize :: BigNumber -> Int
getBigNSize bn = length (digits bn)

{-
instance Show BigNumber where 
  show (BigNumber b l)
    | b = show l
    | otherwise = "-" ++ show l
    -}
  
scanner :: String -> BigNumber
scanner s = intToBigN (read s::Int)

bigNToString :: BigNumber -> String -> String
bigNToString bn s 
  | getBigNSize bn == 1 = s ++ show (head (digits bn))
  | otherwise = bigNToString (BigNumber (sign bn) (tail (digits bn))) (s ++ show (head (digits bn)))

output :: BigNumber -> String
output bn
  | sign bn = bigNToString bn ""
  | otherwise = bigNToString bn "-"


zeroStuffing :: BigNumber -> Int -> BigNumber
zeroStuffing bn nz
  | nz == 1 = BigNumber (sign bn) (0 : digits bn)
  | otherwise = zeroStuffing (BigNumber (sign bn) (0 : digits bn)) (nz-1)

carrySum :: Int -> Int -> (Int, Int)
carrySum n1 n2 = (div (n1 + n2) 10 ,mod (n1 + n2) 10)

{-
sumTuplesToBigN :: [(Int, Int)] -> BigNumber -> BigNumber
sumTuplesToBigN l bn
  | length l == 1 = if
    |first (head l) == 1 -> BigNumber (sign bn) ((second (head l) + first ()  digits bn)))
    |otherwise -> BigNumber (sign bn) ((second (head (tail l))) : digits bn)
  | otherwise = if
    |first (head l) == 1 -> sumTuplesToBigN (tail l) BigNumber (sign bn) ((second (head (tail l)) + 1) : digits bn)
    |otherwise -> sumTuplesToBigN (tail l) (BigNumber (sign bn) ((second (head (tail l))) : digits bn))

parseSumTuples :: [(Int, Int)] -> BigNumber
parseSumTuples l = -}

