# PFL-BigNumbers

Haskell programming project about handling big numbers in lists

## Function List

### Fib.hs

> FIB WITH INT

~~~~hs    
fibRec :: (Integral a) => a -> a -- Returns the nth Fibonacci number using a simple recursive approach 
~~~~
~~~~hs    
fibLista :: Int -> Int -- Returns the nth Fibonacci number using a dynamic programming approach
~~~~
~~~~hs    
fibListaInfinita :: [Integer] -- Generates an infinite list of Fibonacci numbers and returns the nth Fibonacci number 
~~~~
~~~~hs    
fibMinMax :: Int -> Int -> [Integer] -- Returns an interval of Fibonacci numbers between nmin and nmax
~~~~

> FIB WITH BN

~~~~hs    
fibRecBN :: Int -> BigNumber -- Returns the nth Fibonacci number as a BigNumber using a simple recursive approach
~~~~
~~~~hs    
fibRecBNBN :: BigNumber -> BigNumber -- Returns the nth Fibonacci number as a BigNumber using a simple recursive approach. Takes a BigNumber as an argument
~~~~
~~~~hs    
fibListaBN :: Int -> BigNumber -- Returns the nth Fibonacci number as a BigNumber using a dynamic programming approach
~~~~
~~~~hs    
fibListaBNBN :: BigNumber -> BigNumber {-- Returns the nth Fibonacci number as a BigNumber using a dynamic programming approach. Takes a BigNumber as an argument
~~~~
~~~~hs    
fibListaInfinitaBN :: [BigNumber] -- Generates an infinite list of Fibonacci numbers and returns the nth Fibonacci number as a BigNumber
~~~~
~~~~hs    
fibMinMaxBN :: Int -> Int -> [BigNumber]  -- Returns an interval of Fibonacci numbers between nmin and nmax as BigNumbers
~~~~

### BigNumber.hs
> GENERAL AUXILIARY FUNCTIONS

~~~~hs    
nDigits :: Int -> Int -- Returns the number of digits of an Int
~~~~
~~~~hs    
buildBN :: Bool -> Int -> [Int] -> BigNumber -- Auxiliary function that builds a BigNumber from a list of Ints and a Bool
~~~~
~~~~hs    
intToBN :: Int -> BigNumber -- Converts an Int into a BigNumber using the buildBN function
~~~~
~~~~hs    
rawBNToInt :: BigNumber -> Int -> Int -- Auxiliary function that builds an Int from a BigNumber. It consumes the BigNumber recursively until its last digit, which is the base case
~~~~
~~~~hs    
bnToInt :: BigNumber -> Int -- Converts a BigNumber into an Int using the rawBNtoInt function
~~~~
~~~~hs    
getBNSize :: BigNumber -> Int -- Returns the number of digits of a BigNumber
~~~~
~~~~hs    
zeroStuffing :: BigNumber -> Int -> BigNumber -- Injects a specified amount of zeros on the left side of a BigNumber
~~~~
~~~~hs    
zeroStuffingExact :: BigNumber -> Int ->BigNumber -- Injects zeros on the left side of a BigNumber until it reaches the size given as argument
~~~~
~~~~hs    
zeroDestuffing :: BigNumber -> BigNumber -- Removes all the unnecessary zeros on the left of a BigNumber
~~~~

> QUICK BN OPERATIONS

~~~~hs    
absBN :: BigNumber -> BigNumber -- Returns the absolute value of a BigNumber
~~~~
~~~~hs    
negativeBN :: BigNumber -> BigNumber -- Returns the negative of the absolute value of a BigNumber
~~~~
~~~~hs    
simBN :: BigNumber -> BigNumber -- Returns the opposite of a BigNumber
~~~~
~~~~hs    
incrementBN :: BigNumber -> BigNumber -- Increments a BigNumber by one unit
~~~~
~~~~hs    
decrementBN :: BigNumber -> BigNumber -- Decrements a BigNumber by one unit
~~~~
~~~~hs    
zipWithBN :: Bool -> BigNumber -> BigNumber -> (Int->Int->Int) -> BigNumber -- Applies the Prelude function zipWith to the digits of a BigNumber, with the operation given as argument 
~~~~
~~~~hs    
mapBN :: Bool -> BigNumber -> (Int->Int) -> BigNumber -- Applies the Prelude function map to the digits of a BigNumber, with the operation given as argument 
~~~~
~~~~hs    
raiseTenBN :: BigNumber -> Int -> BigNumber -- Raises a BigNumber to a power of ten
~~~~

> BN OPERATION AUXILIARY FUNCTIONS

~~~~hs    
carrySum :: Int -> Int -> (Int, Int) -- Creates a Tuple with the format (carry, result digit) from a sum of two digits
~~~~
~~~~hs    
processOperationCarry :: [Int] -> Int -> [Int] -> [Int] {-- Auxiliary Function that processes the operations' digit by digit result. 
 After a raw sum or subtraction is made digit by digit (which might result in some "digits" being negative or bigger than 10) this function interprets
 every partial result and turns it into a list of Ints that can be a proper BigNumber's digits list. It uses a carry variable so that for example when
 a digit is bigger than 10, only the unit's digit is kept and the next digit is incremented by one. When a digit is negative it means that the next one
 should be decremented by one and the digit to be kept is "10 - the original digit" --}
~~~~
~~~~hs    
processOperation :: BigNumber -> BigNumber {-- Wrapper Function that cleans a BigNumber after a raw operation has been made. It combines zeroDestuffing and
 processOperationCarry to make sure that a BigNumber doesn't have unnecessary zeros on the left and "digits" that might be bigger than 10 or negative --}
~~~~
~~~~hs    
rawsomaBN :: BigNumber -> BigNumber -> BigNumber {-- Auxiliary function that sums "blindly" the digits of two BigNumbers into another BigNumber. 
 The provisory BigNumber returned by this functions must then be processed by processOperation function to become the actual result of the sum intended. --}
~~~~
~~~~hs    
rawSubBN :: BigNumber -> BigNumber -> BigNumber {-- Auxiliary function that subtracts "blindly" the digits of two BigNumbers into another BigNumber. 
 The provisory BigNumber returned by this functions must then be processed by processOperation function to become the actual result of the subtraction intended. --}
~~~~
~~~~hs    
rawMulBN :: BigNumber -> BigNumber -> BigNumber -> BigNumber {-- Auxiliary function that multiplies two BigNumbers. 
It uses a result accumulator and everytime the first operand is added to said accumulator, the second operand is decremented. 
It returns the correct BigNumber result to the original operation --}
~~~~
~~~~hs    
rawDivBN :: BigNumber -> BigNumber -> BigNumber -> BigNumber -> (BigNumber, BigNumber) {-- Auxiliary function that divides two BigNumbers.
 Applies the Euclidian Algorithm using a variable to store the remainder and the signs needed, in order to work with absolutes. It keeps subtracting the divisor
 from the dividend until the remainder sits between 0 and the absolute value of the divisor. It returns the correct BigNumber result to the original operation --}
~~~~

> BN STRING FUNCTIONS

~~~~hs    
scanner :: String -> BigNumber -- Converts a string into a BigNumber
~~~~
~~~~hs    
convertBNToString :: BigNumber -> String -> String {-- Auxiliary function that converts a BigNumber
 into a string by consuming the BigNumber's digits and adding them to an accumulator string --}
~~~~
~~~~hs    
output :: BigNumber -> String -- Converts a BigNumber into a string
~~~~

> BN OPERATIONS

~~~~hs    
somaBN :: BigNumber -> BigNumber ->BigNumber -- Performs the sum of two BigNumbers using the rawsomaBN function
~~~~
~~~~hs    
subBN :: BigNumber -> BigNumber ->BigNumber -- Subtracts two BigNumbers using the rawSubBN function
~~~~
~~~~hs    
mulBN :: BigNumber -> BigNumber -> BigNumber -- Multiplies two BigNumbers using the rawMulBN function
~~~~
~~~~hs    
divBN :: BigNumber -> BigNumber -> (BigNumber, BigNumber) -- Divides two BigNumbers using the rawDivBN function
--divBN bn1 bn2 = oldRawDivBN bn1 bn2 bnZero (absBN bn1) True True
~~~~
~~~~hs    
safeDivBN :: BigNumber -> BigNumber -> Maybe (BigNumber, BigNumber) -- Divides two BigNumbers if the second is not zero
~~~~

### Test.hs

> AUXILIARY FUNCTIONS

~~~~hs    
outputTuple :: (BigNumber, BigNumber) -> String
~~~~
~~~~hs    
outputMaybeBN :: Maybe BigNumber -> String
~~~~
~~~~hs    
outputMaybeTuple :: Maybe (BigNumber, BigNumber) -> String -> String
~~~~

> HELPER FUNCTIONS TO TEST Fib.hs FUNCTIONS

~~~~hs    
testFibRec :: IO () -- Helper funtion to make testing fibRec easier
~~~~
~~~~hs    
testfibLista :: IO () -- Helper funtion to make testing fibLista easier
~~~~
~~~~hs    
testfibListaInfinita :: IO () -- Helper funtion to make testing fibListaInfinita easier
~~~~
~~~~hs    
testFibRecBN :: IO () -- Helper funtion to make testing fibRecBN easier
~~~~
~~~~hs    
testFibRecBNBN :: IO () -- Helper funtion to make testing fibRecBNBN easier
~~~~
~~~~hs    
testfibListaBNBN :: IO () -- Helper funtion to make testing fibListaBNBN easier
~~~~
~~~~hs    
testfibListaInfinitaBN :: IO () -- Helper funtion to make testing fibListaInfinitaBN easier
~~~~

> HELPER FUNCTIONS TO TEST BigNumber.hs FUNCTIONS

~~~~hs
testSomaBN :: IO () -- Helper funtion to make testing somaBN easier
~~~~
~~~~hs
testSubBN :: IO () -- Helper funtion to make testing subBN easier
~~~~
~~~~hs
testMulBN :: IO () -- Helper funtion to make testing mulBN easier
~~~~
~~~~hs
testDivBN :: IO () -- Divides two BigNumbers using the rawDivBN function
~~~~
~~~~hs
testSafeDivBN :: IO () -- Divides two BigNumbers using the rawDivBN function
~~~~


## BigNumber's Operations Strategy
### BigNumber Declaration
~~~~hs    
data BigNumber = BigNumber {sign :: Bool, digits :: [Int]} deriving (Eq, Read)
~~~~

BigNumbers are defined as the combination of a `sign` which is a `Bool` and a list of `digits` represented as `[Int]`. The `sign` is `True` when the BigNumber is positive and it's `False` when the BigNumber is negative.

### scanner
~~~~hs    
scanner :: String -> BigNumber -- Converts a string into a BigNumber
~~~~
In this function we want to convert a string into a BigNumber. We want to accept the following formats:

<ul>
  <li>"123" &nbsp -> 123</li>
  <li>"+123" -> 123</li>
  <li>"-123" -> -123</li>
</ul>

So we know that the BigNumber will only be negative if the head of the string is the character `'-'`, in every other valid case it will be positive. For the digits we use `map (read . pure :: Char -> Int)` on the remainder of the string, ignoring the sign characters (both `'-'` and `'+'`).

### output
~~~~hs    
output :: BigNumber -> String -- Converts a BigNumber into a string
~~~~
The goal of the `ouput` function is to convert a BigNumber into a string. We opted to convert a postive BigNumber without adding to the result string the `'+'` character, while naturally when the BigNumber is negative it's preceded by `'-'`. So if the BigNumber is negative, we concatenate `'-'` in the start of the result string. For the digits we use `concatMap show digits`, so that the `show` function converts each `Int` to `String` and `concatMap` puts the string all together.

### processOperations
~~~~hs    
processOperation :: BigNumber -> BigNumber {-- Wrapper Function that cleans a BigNumber after a raw operation has been made. It combines zeroDestuffing and processOperationCarry to make sure that a BigNumber doesn't have unnecessary zeros on the left and "digits" that might be bigger than 10 or negative --}
~~~~

`processOperations` aaanfs

### somaBN
~~~~hs    
somaBN :: BigNumber -> BigNumber ->BigNumber -- Performs the sum of two BigNumbers using the rawSomaBN function
~~~~
The goal of the `ouput` function is to convert a BigNumber into a string. We opted to convert a postive BigNumber without adding to the result string the `'+'` character, while naturally when the BigNumber is negative it's preceded by `'-'`. So if the BigNumber is negative, we concatenate `'-'` in the start of the result string. For the digits we use `concatMap show digits`, so that the `show` function converts each `Int` to `String` and `concatMap` puts the string all together.

## Int vs BigNumber in Fibonacci Functions

93 causes program to overflow -- fibLista Int -> int
## Test Cases