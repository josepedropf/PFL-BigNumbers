module BigNumber (BigNumber, bnZero, bnOne, bnTwo, scanner, sumBN, subBN, mulBN, divBN, safeDivBN, zipWithBN, mapBN, incrementBN, decrementBN, output, simBN, absBN, negativeBN) where
--type BigNumber = [Int]
--type Sign = Bool
--type Digits = [Int]

data BigNumber = BigNumber {sign :: Bool, digits :: [Int]} deriving (Eq, Read)

bnZero :: BigNumber --Constant
bnZero = BigNumber True [0]

bnOne :: BigNumber --Constant
bnOne = BigNumber True [1]

bnTwo :: BigNumber --Constant
bnTwo = BigNumber True [2]

nDigits :: Int -> Int
nDigits n
  | n < 0 = nDigits (abs n)
  | abs n >= 10 = 1 + nDigits (div n 10)
  | otherwise = 1

buildBN :: Bool -> Int -> [Int] -> BigNumber
buildBN b n l
  | nDigits n == 1 = BigNumber b ((abs n):l)
  | otherwise = buildBN b (div (abs n) 10) (mod (abs n) 10 : l)

intToBN :: Int -> BigNumber
intToBN n
  | n == 0 = bnZero
  | n < 0 = buildBN False n []
  | otherwise = buildBN True n []

instance Show BigNumber where
  show (BigNumber b l)
    | b = "+" ++ show l -- b = show l
    | otherwise = "-" ++ show l

getBNSize :: BigNumber -> Int
getBNSize bn = length (digits bn)

instance Ord BigNumber where
  compare (BigNumber s1 l1) (BigNumber s2 l2)
    | s1 && (not s2) = GT
    | (not s1) && s2 = LT
    | length zdl1 /= length zdl2 = compare (length zdl1) (length zdl2)
    | otherwise =  compare zdl1 zdl2
    where zdl1 = (digits (zeroDestuffing (BigNumber s1 l1)))
          zdl2 = (digits (zeroDestuffing (BigNumber s2 l2)))

oldScanner :: String -> BigNumber
oldScanner s = intToBN (read s::Int)

scanner :: String -> BigNumber
scanner s 
  | (read s::Int) == 0 = bnZero
  | head s == '-' = BigNumber False (map (read . pure :: Char -> Int) (tail s))
  | head s == '+' = BigNumber True (map (read . pure :: Char -> Int) (tail s))
  | otherwise = BigNumber True (map (read . pure :: Char -> Int) s)

convertBNToString :: BigNumber -> String -> String
convertBNToString bn s
  | getBNSize bn == 1 = s ++ show (head (digits bn))
  | otherwise = convertBNToString (BigNumber (sign bn) (tail (digits bn))) (s ++ show (head (digits bn)))

output :: BigNumber -> String
output bn
  | sign bn = convertBNToString bn ""
  | otherwise = convertBNToString bn "-"


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
  | head (digits bn) <= 0 = zeroDestuffing (BigNumber (sign bn) (tail (digits bn)))
  | otherwise = bn

absBN :: BigNumber -> BigNumber
absBN bn = BigNumber True (digits bn)

negativeBN :: BigNumber -> BigNumber
negativeBN bn = BigNumber False (digits bn)

simBN :: BigNumber -> BigNumber
simBN bn = BigNumber (not (sign bn)) (digits bn)

incrementBN :: BigNumber -> BigNumber
incrementBN bn = sumBN bn (BigNumber True [1])

decrementBN :: BigNumber -> BigNumber
decrementBN bn = subBN bn (BigNumber True [1])

zipWithBN :: Bool -> BigNumber -> BigNumber -> (Int->Int->Int) -> BigNumber
zipWithBN s bn1 bn2 op = zeroDestuffing (BigNumber s (zipWith (op) (digits (zeroStuffingExact bn1 nz)) (digits (zeroStuffingExact bn2 nz))) )
  where nz = (max (length (digits bn1)) (length (digits bn2))) + 1

mapBN :: Bool -> BigNumber -> (Int->Int) -> BigNumber
mapBN s bn op = BigNumber s (map (op) (digits bn))

rawSumBN :: BigNumber -> BigNumber -> BigNumber
rawSumBN bn1 bn2
  | (sign bn1) && (sign bn2) = BigNumber True (zipWith (+) (digits (zeroStuffingExact bn1 nz)) (digits (zeroStuffingExact bn2 nz)) )
  | (not (sign bn1)) && (not (sign bn2)) = BigNumber False (zipWith (+) (digits (zeroStuffingExact bn1 nz)) (digits (zeroStuffingExact bn2 nz)) )
  | (sign bn1) && (not (sign bn2)) = rawSubBN bn1 (absBN bn2)
  | (not (sign bn1)) && (sign bn2) = rawSubBN bn2 (absBN bn1)
  where nz = (max (length (digits bn1)) (length (digits bn2))) + 1

rawSubBN :: BigNumber -> BigNumber -> BigNumber
rawSubBN bn1 bn2
  | bn1 == bn2 = bnZero
  | (sign bn1) && (sign bn2) = (if bn1 < bn2
    then negativeBN (rawSubBN bn2 bn1)
    else (BigNumber True (zipWith (-) (digits (zeroStuffingExact bn1 nz)) (digits (zeroStuffingExact bn2 nz))) ))
  | (not (sign bn1)) && (not (sign bn2)) = rawSubBN (absBN bn2) (absBN bn1)
  | (sign bn1) && (not (sign bn2)) = rawSumBN bn1 (absBN bn2)
  | (not (sign bn1)) && (sign bn2) = negativeBN (rawSumBN (absBN bn1) bn2)
  where nz = (max (length (digits bn1)) (length (digits bn2))) + 1


carrySum :: Int -> Int -> (Int, Int)
carrySum n1 n2 = (div (n1 + n2) 10 ,mod (n1 + n2) 10)

processOperation :: BigNumber -> BigNumber
processOperation bn = zeroDestuffing (BigNumber (sign bn) (processOperationCarry (digits bn) 0 []))

sumBN :: BigNumber -> BigNumber ->BigNumber
sumBN bn1 bn2 = processOperation (rawSumBN bn1 bn2)

subBN :: BigNumber -> BigNumber ->BigNumber
subBN bn1 bn2 = processOperation (rawSubBN bn1 bn2)

processOperationCarry :: [Int] -> Int -> [Int] -> [Int]
processOperationCarry l carry res
  | length l == 0 = res
  | (current >= 0 && current <= 9) = processOperationCarry (reverse(tail (reverse l))) 0 ([current]++res)
  | (current >= 10) = processOperationCarry (reverse(tail (reverse l))) 1 ([(mod current 10)]++res)
  | (current < 0) = processOperationCarry (reverse(tail (reverse l))) (-1) ([(current + 10)]++res)
  where current = (last l) + carry

raiseTenBN :: BigNumber -> Int -> BigNumber
raiseTenBN bn power
  | power <= 0 = bn
  | power == 1 = BigNumber (sign bn) ((digits bn)++[0])
  | otherwise = raiseTenBN (BigNumber (sign bn) ((digits bn)++[0])) (power - 1)

rawMulBN :: BigNumber -> BigNumber -> BigNumber -> Int -> BigNumber
rawMulBN bn mul_bn res_bn power
  | bn == bnZero || mul_bn == bnZero = bnZero
  | (not (sign bn)) && (not (sign mul_bn)) = BigNumber True (digits (rawMulBN (absBN bn) (absBN mul_bn) res_bn power))
  | (sign bn) /= (sign mul_bn) = BigNumber False (digits (rawMulBN (absBN bn) (absBN mul_bn) res_bn power))
  | absBN mul_bn > absBN bn = rawMulBN mul_bn bn res_bn power
  | getBNSize mul_bn == 0 = res_bn
  | getBNSize mul_bn == 1 = sumBN res_bn (raiseTenBN (processOperation(mapBN True bn (*(last (digits mul_bn))))) power)
  | otherwise = rawMulBN bn (BigNumber (sign mul_bn) (reverse(tail (reverse (digits mul_bn))))) (sumBN res_bn (raiseTenBN (processOperation(mapBN True bn (*(last (digits mul_bn))))) power)) (power + 1)

mulBN :: BigNumber -> BigNumber -> BigNumber
mulBN bn1 bn2 = zeroDestuffing (rawMulBN bn1 bn2 bnZero 0)

rawDivBN :: BigNumber -> BigNumber -> BigNumber -> BigNumber -> Bool -> Bool -> (BigNumber, BigNumber)
rawDivBN bn div_bn quo_bn rem_bn quo_sign rem_sign
  | bn == bnZero = (bnZero, bnZero)
  | absBN div_bn > absBN bn = (bnZero, BigNumber rem_sign (digits bn))
  | (not (sign bn)) && (not (sign div_bn)) = rawDivBN (absBN bn) (absBN div_bn) quo_bn rem_bn True (sign bn)
  | (sign bn) /= (sign div_bn) = rawDivBN (absBN bn) (absBN div_bn) quo_bn rem_bn False (sign bn)
  | rem_bn == bnZero = (BigNumber quo_sign (digits quo_bn), bnZero)
  | rem_bn > bnZero && rem_bn < (absBN div_bn) = (BigNumber quo_sign (digits quo_bn), BigNumber rem_sign (digits rem_bn))
  | otherwise = rawDivBN bn div_bn (incrementBN quo_bn) (subBN (absBN rem_bn) (absBN div_bn)) quo_sign rem_sign

divBN :: BigNumber -> BigNumber -> (BigNumber, BigNumber)
divBN bn1 bn2 = rawDivBN bn1 bn2 bnZero (absBN bn1) True True

safeDivBN :: BigNumber -> BigNumber -> Maybe (BigNumber, BigNumber)
safeDivBN bn1 bn2
    | bn2 == bnZero = Nothing
    | otherwise = Just (divBN bn1 bn2)











--End
