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
  | nDigits n == 1 = BigNumber b ((abs n):l)
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

instance Ord BigNumber where
  compare (BigNumber s1 l1) (BigNumber s2 l2)
    | s1 && (not s2) = GT
    | (not s1) && s2 = LT
    | length zdl1 /= length zdl2 = compare (length zdl1) (length zdl2)
    | otherwise =  compare zdl1 zdl2
    where zdl1 = (digits (zeroDestuffing (BigNumber s1 l1)))
          zdl2 = (digits (zeroDestuffing (BigNumber s2 l2)))

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
  | nz <= 0 = bn
  | nz == 1 = BigNumber (sign bn) (0 : digits bn)
  | otherwise = zeroStuffing (BigNumber (sign bn) (0 : digits bn)) (nz-1)

zeroStuffingExact :: BigNumber -> Int ->BigNumber
zeroStuffingExact bn nz = zeroStuffing bn (nz - (length (digits bn)))

zeroDestuffing :: BigNumber -> BigNumber
zeroDestuffing bn
  | length (digits bn) == 1 = bn
  | head (digits bn) == 0 = zeroDestuffing (BigNumber (sign bn) (tail (digits bn)))
  | otherwise = bn

absBn :: BigNumber -> BigNumber
absBn bn = BigNumber True (digits bn)

negativeBn :: BigNumber -> BigNumber
negativeBn bn = BigNumber False (digits bn)

simBn :: BigNumber -> BigNumber
simBn bn = BigNumber (not (sign bn)) (digits bn)

rawSumBn :: BigNumber -> BigNumber -> BigNumber
rawSumBn bn1 bn2
  | (sign bn1) && (sign bn2) = BigNumber True (zipWith (+) (digits (zeroStuffingExact bn1 nz)) (digits (zeroStuffingExact bn2 nz)) )
  | (not (sign bn1)) && (not (sign bn2)) = BigNumber False (zipWith (+) (digits (zeroStuffingExact bn1 nz)) (digits (zeroStuffingExact bn2 nz)) )
  | (sign bn1) && (not (sign bn2)) = rawSubBn bn1 (absBn bn2)
  | (not (sign bn1)) && (sign bn2) = rawSubBn bn2 (absBn bn1)
  where nz = (max (length (digits bn1)) (length (digits bn2))) + 1

rawSubBn :: BigNumber -> BigNumber -> BigNumber
rawSubBn bn1 bn2
  | bn1 == bn2 = (scanner "0")
  | (sign bn1) && (sign bn2) = (if bn1 < bn2
    then negativeBn (rawSubBn bn2 bn1)
    else (BigNumber True (zipWith (-) (digits (zeroStuffingExact bn1 nz)) (digits (zeroStuffingExact bn2 nz))) ))
  | (not (sign bn1)) && (not (sign bn2)) = rawSubBn (absBn bn2) (absBn bn1)
  | (sign bn1) && (not (sign bn2)) = rawSumBn bn1 (absBn bn2)
  | (not (sign bn1)) && (sign bn2) = negativeBn (rawSumBn (absBn bn1) bn2)
  where nz = (max (length (digits bn1)) (length (digits bn2))) + 1


carrySum :: Int -> Int -> (Int, Int)
carrySum n1 n2 = (div (n1 + n2) 10 ,mod (n1 + n2) 10)

processOperation :: BigNumber -> BigNumber
processOperation bn = zeroDestuffing (BigNumber (sign bn) (processOperationCarry (digits bn) 0 []))

sumBN :: BigNumber -> BigNumber ->BigNumber
sumBN bn1 bn2 = processOperation (rawSumBn bn1 bn2)

subBN :: BigNumber -> BigNumber ->BigNumber
subBN bn1 bn2 = processOperation (rawSubBn bn1 bn2)

processOperationCarry :: [Int] -> Int -> [Int] -> [Int]
processOperationCarry l carry res
  | length l == 0 = res
  | (current >= 0 && current <= 9) = processOperationCarry (reverse(tail (reverse l))) 0 ([current]++res)
  | (current >= 10) = processOperationCarry (reverse(tail (reverse l))) 1 ([(mod current 10)]++res)
  | (current < 0) = processOperationCarry (reverse(tail (reverse l))) (-1) ([(current + 10)]++res)
  where current = (last l) + carry

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
