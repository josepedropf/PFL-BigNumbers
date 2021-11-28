# PFL-BigNumbers

Haskell programming project about handling big numbers in lists.    
Developed by José Pedro Peixoto Ferreira (up201904515) and José Frederico Rodrigues (up201807626)    

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
fibRecIntBN :: Int -> BigNumber -- Returns the nth Fibonacci number as a BigNumber using a simple recursive approach
~~~~
~~~~hs    
fibRecBN :: BigNumber -> BigNumber -- Returns the nth Fibonacci number as a BigNumber using a simple recursive approach. Takes a BigNumber as an argument
~~~~
~~~~hs    
fibListaIntBN :: Int -> BigNumber -- Returns the nth Fibonacci number as a BigNumber using a dynamic programming approach
~~~~
~~~~hs    
fibListaBN :: BigNumber -> BigNumber {-- Returns the nth Fibonacci number as a BigNumber using a dynamic programming approach. Takes a BigNumber as an argument
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
~~~~hs    
getCloseBN :: BigNumber -> BigNumber -> (BigNumber, BigNumber) {-- Returns the closest possible BigNumber to the target, obtainable by combining
 the getCloseBNPower and getCloseBNMult functions on the base BigNumber --}
~~~~
~~~~hs    
getCloseBNPower :: BigNumber -> Int -> BigNumber -> (BigNumber, BigNumber) -- Returns the closest possible BigNumber to the target, obtainable by using the raiseTenBN function on the base BigNumber
~~~~
~~~~hs    
getCloseBNMult :: BigNumber -> BigNumber -> BigNumber -> (BigNumber, BigNumber) -- Returns the closest possible BigNumber to the target, obtainable by multiplying the base BigNumber
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
testFibRecIntBN :: IO () -- Helper funtion to make testing fibRecBN easier
~~~~
~~~~hs    
testFibRecBN :: IO () -- Helper funtion to make testing fibRecBNBN easier
~~~~
~~~~hs    
testfibListaBN :: IO () -- Helper funtion to make testing fibListaBNBN easier
~~~~
~~~~hs    
testFibListaIntBN :: IO () -- Helper funtion to make testing fibListaIntBN easier
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
  <li>"123" <=> 123</li>
  <li>"-123" <=> -123</li>
</ul>

So we know that the BigNumber will only be negative if the head of the string is the character `'-'`, in every other valid case it will be positive. For the digits we use `map (read . pure :: Char -> Int)` on the remainder of the string, ignoring the sign characters.

### output
~~~~hs    
output :: BigNumber -> String -- Converts a BigNumber into a string
~~~~
The goal of the `ouput` function is to convert a BigNumber into a string. We opted to convert a postive BigNumber without adding to the result string the `'+'` character, while naturally when the BigNumber is negative it's preceded by `'-'`. So if the BigNumber is negative, we concatenate `'-'` in the start of the result string. For the digits we use `concatMap show digits`, so that the `show` function converts each `Int` to `String` and `concatMap` puts the string all together.

### processOperation
~~~~hs        
processOperation :: BigNumber -> BigNumber {-- Wrapper Function that cleans a BigNumber after a raw operation has been made.
It combines zeroDestuffing and processOperationCarry to make sure that a BigNumber doesn't have unnecessary zeros on the left and "digits" that might be bigger than 10 or negative --}
~~~~
`processOperation` is a function that "normalizes" a BigNumber returned by either the rawSomaBN or the rawSubBN functions. It essentially is a wrapper to ` processOperationCarry` that also applies the `zeroDestuffing` method and by doing this ensures there are no unnecessary zeros on the leftmost part of the BigNumber. The `processOperationCarry` as it's own name indicates, processes "digits" (although they really aren't digits at this point, as it's not guaranteed that they are in the interval [1..9]) that are either greater than 10 or negative. If the "digit" is greater than 10, it means only the unit part should reamin and a unit must be carried to the next "digit". In the other hand, if the "digit" is negative, this function carries subtracts one from the next "digit" while keeping only the difference between ten and the unprocessed "digit" as the actual result for that position on the list. We thought this approach was a good fit for the task in hand as it works with both sum and subtraction and keeps these functions quite simple (basically just a `zipWith`). 

### somaBN
~~~~hs    
somaBN :: BigNumber -> BigNumber ->BigNumber -- Performs the sum of two BigNumbers using the rawSomaBN function
~~~~
This funtion adds two BigNumbers. It basically uses `processOperation` on the result given by `rawSomaBN`. The function `rawSomaBN` handles the signs of the operands by trying to simplify every sign combination to either a simple sum or subtraction, that is, an operation with two positive BigNumbers:

<ul>
  <li><strong>bn1 + bn2</strong> is the base sum and <strong>bn1 - bn2</strong> is the base subtraction</li>
  <li>(-bn1) + (-bn2) <=> - (bn1 + bn2)</li>
  <li>bn1 + (-bn2) <=> bn1 - bn2</li>
  <li>(-bn1) + bn2 <=> bn2 - bn1</li>
</ul>

The base addition (with two positive BigNumbers) is done via a `zipWith (+)` with the lists of digits, after they are stuffed with enough zeros (if any) on the left to make sure the `zipWith` works properly.

### subBN
~~~~hs    
subBN :: BigNumber -> BigNumber ->BigNumber -- Subtracts two BigNumbers using the rawSubBN function
~~~~
This funtion subtracts BigNumbers. It basically uses `processOperation` on the result given by `rawSubBN`. The function `rawSubBN` handles the signs of the operands by trying to simplify every sign combination to either a simple sum or subtraction, that is, an operation with two positive BigNumbers:

<ul>
  <li><strong>bn1 + bn2</strong> is the base sum and <strong>bn1 - bn2</strong> is the base subtraction</li>
  <li>(-bn1) - (-bn2) <=> bn2 - bn1</li>
  <li>bn1 - (-bn2) <=> bn1 + bn2</li>
  <li>(-bn1) - bn2 <=> - (bn1 + bn2)</li>
</ul>

The base subtraction (with two positive BigNumbers) is done via a `zipWith (-)` with the lists of digits, after they are stuffed with enough zeros (if any) on the left to make sure the `zipWith` works properly. This functions also forces the greater operand to be the minuend (the first one) so that in the base subtraction we know that the result's sign will always be positive.

### mulBN
~~~~hs    
mulBN :: BigNumber -> BigNumber -> BigNumber -- Multiplies two BigNumbers using the rawMulBN function
~~~~

This funtion multiplies BigNumbers. It basically uses processOperation on the result given by rawMulBN. The function `rawSubBN` handles the signs of the operands by trying to simplify every sign combination to a multiplication with two positive BigNumbers:

<ul>
  <li><strong>bn1 * bn2</strong> is the base multiplication</li>
  <li>(-bn1) * (-bn2) <=> bn1 * bn2</li>
  <li>bn1 * (-bn2) <=> - (bn1 * bn2)</li>
  <li>(-bn1) * bn2 <=> - (bn1 * bn2)</li>
</ul>

The base multiplication (with two positive BigNumbers) is done by multiplying the first BigNumber by every digit of the second (with the help of the `mapBN` auxiliary function) and adding it to the result accumulator (having in mind the power of ten of each digit of the second argument). `processOperation` is needed for processing the result of the `mapBN`. To make it easier to multiply BigNumbers by 10 we used the auxialiary function `raiseTenBN`. rawMulBN also forces the greater operand to be the first argument (the "static" one) to improve performance.

### divBN
~~~~hs    
divBN :: BigNumber -> BigNumber -> (BigNumber, BigNumber) -- Divides two BigNumbers using the rawDivBN function
~~~~

This function divides BigNumbers. It's basically a wrapper to the `rawDivBN` function that correctly initializes the remainder. It returns a tuple withe format **"(quotient, remaider)"**. The function `rawDivBN` handles the signs of the operands by trying to simplify every sign combination to a division of two positive BigNumbers:

<ul>
  <li><strong>bn1 / bn2</strong> is the base division with the base output: <strong>(quo, rem)</strong></li>
  <li>(-bn1) / (-bn2) <=>  bn1 / bn2 => (quo, -rem)</li>
  <li>bn1 / (-bn2) <=>  bn1 / bn2 => (-quo, rem)</li>
  <li>(-bn1) / bn2 <=>  bn1 / bn2 => (-quo, -rem)</li>
</ul>

The base division (with two positive BigNumbers) is done by following the Euclidian Algorithm, with a headstart. The headstart is done with the help of the `getCloseBN`. In turn the function `getCloseBN` uses a trial and error approach to find the closest BigNumber of the type `a * 10^n, 0 <= a <= 9` to another target BigNumber. We chose to do this auxiliary function as it's very efficient (in the worse case it does less than 20 simple recursive calls) while providing a noticiable boost to `rawDivBN`. 
The quotient is set to the headstart and the remainder is calculated according to the refered quotient. Then we start subtracting the dividend to the remainder (and incrementing the quotient accordingly) until this last is placed between zero and the dividend itself. When that happens we return the result tuple consisting as said before of the quotient an the remainder.

## Test Cases
> Fib.hs functions

| **Function** | **Test Description** | **Input** | **Result** |
| --------------------------|----------------------------------------------------------------------------------------------------------------------------------------|  ----------------------------|----------------------------|
| fibRec                    | This function should return the fibonacci number at index 9 | 9              |34   |
| fibRec                    | This function should return the fibonacci number at index 20 | 20             |6765 |
| fibrec                    | This function should return the fibonacci number at index 0  | 0              |0    |
| fibLista                  | This function should return the fibonacci number at index 35             | 35             |9227465   | 
| fibLista                  | This function should return the fibonacci number at index 53             | 53             |53316291173  |
| fibListaInfinita          | This function should return the fibonacci number at index 120             | 120        |5358359254990966640871840|
| fibListaInfinita          | This function should return the fibonacci number at index 300             | 300        |222232244629420445529739893461909967206666939096499764990979600|
| fibListaInfinita                    | This function should return the fibonacci number at index 2000    | 2000 |4224696333392304878706725602341482782579852840250681098010280137314308584370130707224123599639141511088446087538909603607640194711643596029271983312598737326253555802606991585915229492453904998722256795316982874482472992263901833716778060607011615497886719879858311468870876264597369086722884023654422295243347964480139515349562972087652656069529806499841977448720155612802665404554171717881930324025204312082516817125|
| fibRecBN                   | This function should return the fibonacci number at index 19     | 19 |4181 |
| fibListaBN   | This function should return the fibonacci number at index 46 | 46 | 1836311903 |
| fibListaBN   | This function should return the fibonacci number at index 70 | 70 | 190392490709135 |
| fibListaBN   | This function should return the fibonacci number at index 210 | 210 | 34507973060837282187130139035400899082304280 |
| fibListaBN   | This function should return the fibonacci number at index 2100 | 2100 |334625877289438178855843463994153787720905326505376699030526482894853127061238245339731427306306593576574686385435616220452221617648924713722274376806346263221160352325559428627803912238856439474915110073472671275860442207216749054884566550138031425342961326523l1899378789412217095899219158241009830329440878555524678295655938399582702177130182044596966419762299111936890305975459231961735528447500193845931171707920809046738569678456857200|
| fibListaInfinitaBN | This function should return the fibonacci number at index 500 | 500 |139423224561697880139724382870407283950070256587697307264108962948325571622863290691557658876222521294125 |
| fibListaInfinitaBN | This function should return the fibonacci number at index 1300 | 1300 | 21599680283161715807847052066540433422883515772119658063766498972503219104278316186542706552263614678844605521205471865945806520838603391933189946547621953603163789045147079719349493433360218263689302235202664706161893962580201172846238976101277970849319269574650368333475 |

>BigNumber.hs functions

| **Function** | **Test Description** | **Input** | **Result** |
| --------------------------|----------------------------------------------------------------------------------------------------------------------------------------|  ----------------------------|----------------------------|
| scanner                    | The string provided as input should be converted into a BigNumber             | "123456789"   |+[1,2,3,4,5,6,7,8,9]|
| scanner                    | The string provided should be converted into a BigNumber            | "-9187488141"|-[9,1,8,7,4,8,8,1,4,1]|
| output                    | The BigNumber provided as input should be converted into a string             | (scanner "32132")  |"32132" |
| somaBN                    | Performing the sum of two positive numbers   |  1349959100591951995183, 1959886991889681              |  1349961060478943884864 |
| somaBN                    |Performing the sum of two negative numbers     |-95288910591, -001591883571   |-96880794162 |
| somaBN                    |Performing the sum of a negative number with a positive number   |-398885882, 678825728  |279939846 |
| somaBN                    |Performing the sum of a small negative number with a small positive number     |-130, 67  |-63 |
| subBN                  | Subtracting two positive numbers    |885775175,  115485991554  |-106628266379  | 
| subBN                  | Subtracting two negative numbers    |-1545992569, -29599922959  |28053930390  | 
| subBN                  | Subtracting a positive number from a negative number    |-155835776837, 98277589827553  | -98433425604390|
| subBN                  | Subtracting two small positive numbers    |6,2  | 4| 
| mulBN                  | Multiplying two positive numbers    | 98578827, 146 |14392508742  |
| mulBN                  | Multiplying a negative number and a positive number    | -88699328, 873 |-77434513344 |
| mulBN                  | Multiplying two negative numbers    | -1577357, -982 |1548964574 |
| mulBN                  | Multiplying a small negative number with a small positive number    | -20, 5 |-100 |
| mulBN                  | Multiplying a positive number with 0    | 20, 0 |0 |
| divBN          | Dividing two positive numbers         |1898528, 240  |(7910, 128)|
| divBN          | Dividing a positive number by a negative number         | 87688275, -872  |(-100559, 827)|
| divBN          | Dividing two negative numbers         | -58385667, -1425 |(40972, -567)|
| divBN          | Dividing a negative number by a greater negative number | -25667, -158863782 |(0, -25667)|
| safeDivBN          | Dividing a positive number by zero      | 5871758917, 0        |Math Error -> Division by Zero|
| safeDivBN          | Dividing a positive number by a negative number     | 187682758,  -9865| (-19025, 1133)|


## Int vs Integer vs BigNumber in Fibonacci Functions

| **Function** | **Argument and return value type** | **Behaviour** |
| --------------------------|--------------------------------------------------------------------------------|  ----------------------------|
| fibRec             | Integral | The fibRec function is not very efficient for bigger indexes, and becomes extremely slow very quickly (at about 35) |
| fibRecBN           | BigNumber | The fibRecBN function is also not very efficient for bigger indexes, becoming extremely slow very quickly (at about 35) |
| fibLista           | Integral | With Int at index 93 or above the function overflows, with Integer the function does not seem to overflow and is pverall very time efficient |
| fibListaBN         | BigNumber | fibListaBN is more efficient that the Int counterpart as it does not overflow, yet not as efficient as the Integer version of the function, because it starts to be really slow around the 6000th Fibonacci number |
| fibListaInfinita   | Integer | With Integer it does not overflow and is the fastest of all the implementations. The process is killed by ghci at around index 500000 |
| fibListaInfinitaBN | BigNumber | Just like its Integer counterpart this functions doesn't overflow, but it does get really slow at indexes near the 6000 |

In conclusion, the `Int` type can suffer overflow as it is only guaranteed to work properly in the range [-2^29 .. 2^29], meanwhile neither the `Integers` nor the `BigNumbers` seem to overflow. On the other hand `Int` and `Integer` seem to be more efficient, probably because they are implemented as part of the Prelude, while the `BigNumber` data type is implemented by us. But it's worth noting that developing the `BigNumber` data type and all operations associated with it, was an interesting experience and a very satisfactory one at times :).