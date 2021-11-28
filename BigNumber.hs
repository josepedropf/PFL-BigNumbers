module BigNumber (BigNumber, bnZero, bnOne, bnTwo, scanner, somaBN, subBN, mulBN, divBN, safeDivBN, indexBN, 
  zipWithBN, mapBN, incrementBN, decrementBN, output, simBN, absBN, negativeBN, intToBN, buildBN, bnToInt, rawDivBN) where

{- BN DECLARATION -}
data BigNumber = BigNumber {sign :: Bool, digits :: [Int]} deriving (Read)
-- sign True <=> Positive BigNumber && sign False <=> Negative BigNumber

{- CONSTANTS -}
bnZero :: BigNumber --Constant (=0)
bnZero = BigNumber True [0]

bnOne :: BigNumber --Constant (=1)
bnOne = BigNumber True [1]

bnMinusOne :: BigNumber --Constant (=-1)
bnMinusOne = BigNumber False [1]

bnTwo :: BigNumber --Constant (=2)
bnTwo = BigNumber True [2]


{- BN INSTANCES -}
instance Eq BigNumber where -- Formats the way BigNumbers are printed ('-'|'+'[list of digits])
  (BigNumber b1 l1) == (BigNumber b2 l2)
    | b1 /= b2 = zdl1 == [0] && zdl2 == [0]
    | otherwise = zdl1 == zdl2
    where zdl1 = digits (zeroDestuffing (BigNumber b1 l1))
          zdl2 = digits (zeroDestuffing (BigNumber b2 l2))

instance Show BigNumber where -- Formats the way BigNumbers are printed ('-'|'+'[list of digits])
  show (BigNumber b l)
    | b = "+" ++ show l
    | otherwise = "-" ++ show l

instance Ord BigNumber where -- Handles the comparison between BigNumbers
  compare (BigNumber s1 l1) (BigNumber s2 l2)
    | s1 && not s2 = GT
    | not s1 && s2 = LT
    | not s1 && not s2 = compare (BigNumber True l2) (BigNumber True l1)
    | length zdl1 /= length zdl2 = compare (length zdl1) (length zdl2)
    | otherwise =  compare zdl1 zdl2
    where zdl1 = digits (zeroDestuffing (BigNumber s1 l1))
          zdl2 = digits (zeroDestuffing (BigNumber s2 l2))


{- GENERAL AUXILIARY FUNCTIONS -}
nDigits :: Int -> Int -- Returns the number of digits of an Int
nDigits n
  | n < 0 = nDigits (abs n)
  | abs n >= 10 = 1 + nDigits (div n 10)
  | otherwise = 1

buildBN :: Bool -> Int -> [Int] -> BigNumber -- Auxiliary function that builds a BigNumber from a list of Ints and a Bool
buildBN b n l
  | nDigits n == 1 = BigNumber b (abs n:l)
  | otherwise = buildBN b (div (abs n) 10) (mod (abs n) 10 : l)

intToBN :: Int -> BigNumber -- Converts an Int into a BigNumber using the buildBN function
intToBN n
  | n == 0 = bnZero
  | n < 0 = buildBN False n []
  | otherwise = buildBN True n []

rawBNToInt :: BigNumber -> Int -> Int -- Auxiliary function that builds an Int from a BigNumber. It consumes the BigNumber recursively until its last digit, which is the base case
rawBNToInt bn n
  | getBNSize bn == 1 = if sign bn then n + last (digits bn) else -(n + last (digits bn))
  | otherwise = rawBNToInt (BigNumber (sign bn) (tail (digits bn))) (n + (head (digits bn) * (10 ^ (getBNSize bn - 1))))

bnToInt :: BigNumber -> Int -- Converts a BigNumber into an Int using the rawBNtoInt function
bnToInt bn = rawBNToInt bn 0

getBNSize :: BigNumber -> Int -- Returns the number of digits of a BigNumber
getBNSize bn = length (digits bn)

indexBN :: [BigNumber] -> BigNumber -> BigNumber
indexBN lbn bn = lbn !! bnToInt bn

zeroStuffing :: BigNumber -> Int -> BigNumber -- Injects a specified amount of zeros on the left side of a BigNumber
zeroStuffing bn nz
  | nz <= 0 = bn
  | nz == 1 = BigNumber (sign bn) (0 : digits bn)
  | otherwise = zeroStuffing (BigNumber (sign bn) (0 : digits bn)) (nz-1)

zeroStuffingExact :: BigNumber -> Int ->BigNumber -- Injects zeros on the left side of a BigNumber until it reaches the size given as argument
zeroStuffingExact bn nz = zeroStuffing bn (nz - length (digits bn))

zeroDestuffing :: BigNumber -> BigNumber -- Removes all the unnecessary zeros on the left of a BigNumber
zeroDestuffing bn
  | getBNSize bn == 1 = bn
  | head (digits bn) <= 0 = zeroDestuffing (BigNumber (sign bn) (tail (digits bn)))
  | otherwise = bn


{- QUICK BN OPERATIONS-}
absBN :: BigNumber -> BigNumber -- Returns the absolute value of a BigNumber
absBN bn = BigNumber True (digits bn)

negativeBN :: BigNumber -> BigNumber -- Returns the negative of the absolute value of a BigNumber
negativeBN bn
  | bn == bnZero = bnZero
  | otherwise = BigNumber False (digits bn)

simBN :: BigNumber -> BigNumber -- Returns the opposite of a BigNumber
simBN bn = BigNumber (not (sign bn)) (digits bn)

incrementBN :: BigNumber -> BigNumber -- Increments a BigNumber by one unit
incrementBN bn = somaBN bn bnOne

decrementBN :: BigNumber -> BigNumber -- Decrements a BigNumber by one unit
decrementBN bn = subBN bn bnOne

zipWithBN :: Bool -> BigNumber -> BigNumber -> (Int->Int->Int) -> BigNumber -- Applies the Prelude function zipWith to the digits of a BigNumber, with the operation given as argument 
zipWithBN s bn1 bn2 op = zeroDestuffing (BigNumber s (zipWith op (digits (zeroStuffingExact bn1 nz)) (digits (zeroStuffingExact bn2 nz))) )
  where nz = max (length (digits bn1)) (length (digits bn2)) + 1

mapBN :: Bool -> BigNumber -> (Int->Int) -> BigNumber -- Applies the Prelude function map to the digits of a BigNumber, with the operation given as argument 
mapBN s bn op = BigNumber s (map op (digits bn))

raiseTenBN :: BigNumber -> Int -> BigNumber -- Raises a BigNumber to a power of ten
raiseTenBN bn power
  | power <= 0 = bn
  | power == 1 = BigNumber (sign bn) (digits bn ++ [0])
  | otherwise = raiseTenBN (BigNumber (sign bn) (digits bn ++ [0])) (power - 1)

getCloseBN :: BigNumber -> BigNumber -> (BigNumber, BigNumber) {-- Returns the closest possible BigNumber to the target, obtainable by combining
 the getCloseBNPower and getCloseBNMult functions on the base BigNumber --}
getCloseBN target_bn base_bn = (fst bnMul, mulBN (snd bnPower) (snd bnMul))
  where bnMul = getCloseBNMult target_bn bnTwo (fst bnPower)
        bnPower = getCloseBNPower target_bn 1 base_bn

getCloseBNPower :: BigNumber -> Int -> BigNumber -> (BigNumber, BigNumber) -- Returns the closest possible BigNumber to the target, obtainable by using the raiseTenBN function on the base BigNumber 
getCloseBNPower target_bn power base_bn 
  | base_bn_raised < target_bn = getCloseBNPower target_bn (power + 1) base_bn
  | base_bn_raised == target_bn = (base_bn_raised, raiseTenBN bnOne power)
  | otherwise = (raiseTenBN base_bn (power - 1), raiseTenBN bnOne (power - 1))
  where base_bn_raised = raiseTenBN base_bn power

getCloseBNMult :: BigNumber -> BigNumber -> BigNumber -> (BigNumber, BigNumber) -- Returns the closest possible BigNumber to the target, obtainable by multiplying the base BigNumber
getCloseBNMult target_bn mul_bn base_bn 
  | base_bn_multp < target_bn = getCloseBNMult target_bn (incrementBN mul_bn) base_bn
  | base_bn_multp == target_bn = (base_bn_multp, mul_bn)
  | otherwise = (mulBN base_bn (decrementBN mul_bn), decrementBN mul_bn)
  where base_bn_multp = mulBN base_bn mul_bn


{- BN OPERATION AUXILIARY FUNCTIONS -}
carrySum :: Int -> Int -> (Int, Int) -- Creates a Tuple with the format (carry, result digit) from a sum of two digits
carrySum n1 n2 = (div (n1 + n2) 10 ,mod (n1 + n2) 10)

processOperationCarry :: [Int] -> Int -> [Int] -> [Int] {-- Auxiliary Function that processes the operations' digit by digit result. 
 After a raw sum or subtraction is made digit by digit (which might result in some "digits" being negative or bigger than 10) this function interprets
 every partial result and turns it into a list of Ints that can be a proper BigNumber's digits list. It uses a carry variable so that for example when
 a digit is bigger than 10, only the unit's digit is kept and the next digit is incremented by one. When a digit is negative it means that the next one
 should be decremented by one and the digit to be kept is "10 - the original digit" --}
processOperationCarry l carry res
  | null l = res
  | otherwise = processOperationCarry (init l) new_carry (mod current 10 : res)
  where current = last l + carry
        new_carry = div current 10

processOperation :: BigNumber -> BigNumber {-- Wrapper Function that cleans a BigNumber after a raw operation has been made. It combines zeroDestuffing and
 processOperationCarry to make sure that a BigNumber doesn't have unnecessary zeros on the left and "digits" that might be bigger than 10 or negative --}
processOperation bn
  | bn == bnZero = bnZero
  | otherwise = zeroDestuffing (BigNumber (sign bn) (processOperationCarry (digits bn) 0 []))

rawSomaBN :: BigNumber -> BigNumber -> BigNumber {-- Auxiliary function that sums "blindly" the digits of two BigNumbers into another BigNumber. 
 The provisory BigNumber returned by this functions must then be processed by processOperation function to become the actual result of the sum intended. --}
rawSomaBN bn1 bn2
  | sign bn1 == sign bn2 = BigNumber (sign bn1) (zipWith (+) (digits (zeroStuffingExact bn1 nz)) (digits (zeroStuffingExact bn2 nz)))
  | sign bn1 = rawSubBN bn1 (absBN bn2)
  | otherwise = rawSubBN bn2 (absBN bn1)
  where nz = max (length (digits bn1)) (length (digits bn2)) + 1

rawSubBN :: BigNumber -> BigNumber -> BigNumber {-- Auxiliary function that subtracts "blindly" the digits of two BigNumbers into another BigNumber. 
 The provisory BigNumber returned by this functions must then be processed by processOperation function to become the actual result of the subtraction intended. --}
rawSubBN bn1 bn2
  | sign bn1 && sign bn2 = if bn1 < bn2
      then negativeBN (rawSubBN bn2 bn1)
      else BigNumber True (zipWith (-) (digits (zeroStuffingExact bn1 nz)) (digits (zeroStuffingExact bn2 nz)))
  | not (sign bn1) && not (sign bn2) = rawSubBN (absBN bn2) (absBN bn1)
  | sign bn1 && not (sign bn2) = rawSomaBN bn1 (absBN bn2)
  | not (sign bn1) && sign bn2 = negativeBN (rawSomaBN (absBN bn1) bn2)
  where nz = max (length (digits bn1)) (length (digits bn2)) + 1

oldRawMulBN :: BigNumber -> BigNumber -> BigNumber -> BigNumber {-- Auxiliary function that multiplies two BigNumbers. 
It uses a result accumulator and everytime the first operand is added to said accumulator, the second operand is decremented. 
It returns the correct BigNumber result to the original operation --}
oldRawMulBN bn mul_bn res_bn
  | bn == bnZero || mul_bn == bnZero = bnZero
  | absBN mul_bn > absBN bn = oldRawMulBN mul_bn bn res_bn
  | sign bn /= sign mul_bn = negativeBN absMul
  | not (sign bn) && not (sign mul_bn) = absMul
  | mul_bn == bnOne = somaBN res_bn bn
  | otherwise = oldRawMulBN bn (decrementBN mul_bn) (somaBN res_bn bn)
  where absMul = oldRawMulBN (absBN bn) (absBN mul_bn) res_bn

rawMulBN :: BigNumber -> BigNumber -> BigNumber -> Int -> BigNumber {-- Auxiliary function that multiplies two BigNumbers. It uses a result accumulator
 and another variable to keep track of the power of ten of the next partial multiplication. A partial multiplication is an operation using one digit of the second
 operand. Adding every partial multiplication considering its power of ten will give the correct BigNumber result to the original operation --}
rawMulBN bn mul_bn res_bn power
  | bn == bnZero || mul_bn == bnZero = bnZero
  | not (sign bn) && not (sign mul_bn) = absBN absMul
  | sign bn /= sign mul_bn = negativeBN absMul
  | absBN mul_bn > absBN bn = rawMulBN mul_bn bn res_bn power
  | getBNSize mul_bn == 1 = somaBN res_bn (raiseTenBN (processOperation(mapBN True (zeroStuffing bn 1) (*last (digits mul_bn)))) power)
  | otherwise = rawMulBN bn (BigNumber (sign mul_bn) (init(digits mul_bn))) (somaBN res_bn (raiseTenBN (processOperation(mapBN True (zeroStuffing bn 1) (*last (digits mul_bn)))) power)) (power + 1)
  where absMul = rawMulBN (absBN bn) (absBN mul_bn) res_bn power

oldRawDivBN :: BigNumber -> BigNumber -> BigNumber -> BigNumber -> Bool -> Bool -> (BigNumber, BigNumber) {-- Auxiliary function that divides two BigNumbers.
 Applies the Euclidian Algorithm using a variable to store the remainder and the signs needed, in order to work with absolutes. It keeps subtracting the divisor
 from the dividend until the remainder sits between 0 and the absolute value of the divisor. It returns the correct BigNumber result to the original operation --}
oldRawDivBN bn div_bn quo_bn rem_bn quo_sign rem_sign
  | bn == bnZero = (bnZero, bnZero)
  | absBN div_bn > absBN bn = (bnZero, BigNumber rem_sign (digits bn))
  | not (sign bn) && not (sign div_bn) = oldRawDivBN (absBN bn) (absBN div_bn) quo_bn rem_bn True (sign bn)
  | sign bn /= sign div_bn = oldRawDivBN (absBN bn) (absBN div_bn) quo_bn rem_bn False (sign bn)
  | rem_bn >= bnZero && rem_bn < absBN div_bn = (BigNumber quo_sign (digits quo_bn), BigNumber rem_sign (digits rem_bn))
  | otherwise = oldRawDivBN bn div_bn (incrementBN quo_bn) (subBN (absBN rem_bn) (absBN div_bn)) quo_sign rem_sign

rawDivBN :: BigNumber -> BigNumber -> BigNumber -> BigNumber -> (BigNumber, BigNumber) {-- Auxiliary function that divides two BigNumbers.
 Applies the Euclidian Algorithm using a variable to store the remainder and the signs needed, in order to work with absolutes. It keeps subtracting the divisor
 from the dividend until the remainder sits between 0 and the absolute value of the divisor. It returns the correct BigNumber result to the original operation --}
rawDivBN bn div_bn quo_bn rem_bn
  | bn == bnZero = (bnZero, bnZero)
  | absBN div_bn > absBN bn = (bnZero, bn)
  | not (sign bn) && not (sign div_bn) = (fst absDiv, negativeBN (snd absDiv))
  | sign bn /= sign div_bn = (negativeBN (fst absDiv), BigNumber (sign bn) (digits (snd absDiv)))
  | rem_bn >= bnZero && rem_bn < absBN div_bn = (quo_bn, rem_bn)
  | quo_bn == bnZero = rawDivBN bn div_bn (snd headstart) (subBN (absBN rem_bn) (absBN (fst headstart)))
  | otherwise = rawDivBN bn div_bn (incrementBN quo_bn) (subBN (absBN rem_bn) (absBN div_bn))
  where absDiv = rawDivBN (absBN bn) (absBN div_bn) quo_bn rem_bn
        headstart = getCloseBN (absBN bn) (absBN div_bn)

{- BN STRING FUNCTIONS -}
oldScanner :: String -> BigNumber -- Converts a string into a BigNumber
oldScanner s
  | (read s::Int) == 0 = bnZero
  | head s == '+' = BigNumber True (map (read . pure :: Char -> Int) (tail s))
  | head s == '-' = BigNumber False (map (read . pure :: Char -> Int) (tail s))
  | otherwise = BigNumber True (map (read . pure :: Char -> Int) s)

scanner :: String -> BigNumber -- Converts a string into a BigNumber
scanner s 
  | (read s::Int) == 0 = bnZero
  | otherwise = BigNumber (head s /= '-') (map (read . pure :: Char -> Int) nstring)
  where nstring = if head s == '-' || head s == '+' then tail s else s 

convertBNToString :: BigNumber -> String -> String {-- Auxiliary function that converts a BigNumber
 into a string by consuming the BigNumber's digits and adding them to an accumulator string --}
convertBNToString bn s
  | getBNSize bn == 1 = s ++ show (head (digits bn))
  | otherwise = convertBNToString (BigNumber (sign bn) (tail (digits bn))) (s ++ show (head (digits bn)))

output :: BigNumber -> String -- Converts a BigNumber into a string
output bn = signal ++ concatMap show (digits bn)
  where signal = if sign bn then "" else "-"


{- BN OPERATIONS -}
somaBN :: BigNumber -> BigNumber ->BigNumber -- Performs the sum of two BigNumbers using the rawSomaBN function
somaBN bn1 bn2 = processOperation (rawSomaBN bn1 bn2)

subBN :: BigNumber -> BigNumber ->BigNumber -- Subtracts two BigNumbers using the rawSubBN function
subBN bn1 bn2 = processOperation (rawSubBN bn1 bn2)

mulBN :: BigNumber -> BigNumber -> BigNumber -- Multiplies two BigNumbers using the rawMulBN function
mulBN bn1 bn2 = processOperation (rawMulBN bn1 bn2 bnZero 0)
--mulBN bn1 bn2 = processOperation (oldRawMulBN bn1 bn2 bnZero)

divBN :: BigNumber -> BigNumber -> (BigNumber, BigNumber) -- Divides two BigNumbers using the rawDivBN function
--divBN bn1 bn2 = oldRawDivBN bn1 bn2 bnZero (absBN bn1) True True
divBN bn1 bn2 = rawDivBN bn1 bn2 bnZero (absBN bn1)

safeDivBN :: BigNumber -> BigNumber -> Maybe (BigNumber, BigNumber) -- Divides two BigNumbers if the second is not zero
safeDivBN bn1 bn2
    | bn2 == bnZero = Nothing
    | otherwise = Just (divBN bn1 bn2)


--End