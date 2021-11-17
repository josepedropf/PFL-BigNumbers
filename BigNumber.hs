type BigNumber = [Int]
{-
data Digit = 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9

addDigit :: Digit -> Digit -> Digit
addDigit d1 d2 = mod (d1 + d2) 10
-}

nDigits :: Int -> Int
nDigits n
  | ((mod 10 n) == 10) = 1 + nDigits (div n 10)
  | otherwise = 1

intToBigN_Reverse :: Int -> BigNumber
intToBigN_Reverse n
  | n < 0 = (intToBigN_Reverse (div n 10)) ++ [(-(mod n 10))]
  | otherwise = (intToBigN_Reverse (div n 10)) ++ [(mod n 10)]

{-
intToL :: Int -> [Int]
intToL n
  | n < 0 = (intToL (div n 10)) ++ [(-(mod n 10))]
  | otherwise = (intToL (div n 10)) ++ [(mod n 10)]
  -}

intToL :: Int -> [Int]
intToL n
  | (nDigits n == 1) = [n]
  | n > 0 = (intToL (div n 10)) ++ [(mod n 10)]
  | otherwise = (intToL (-(div (-n) 10))) ++ [-(mod n 10)]

--scanner :: String -> BigNumber
--scanner =
