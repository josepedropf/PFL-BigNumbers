# PFL-BigNumbers

Haskell programming project about handling big numbers in lists

## Function List

### Fib.hs

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



## BigNumber's Operations Strategy

## Int vs BigNumber in Fibonacci Functions

## Test Cases