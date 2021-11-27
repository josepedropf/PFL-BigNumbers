module Fib (fibRec, fibList, fibListInf, fibRecBNBN, fibListBNBN, fibListInfBN) where 
import Distribution.Simple.Program.HcPkg (list)
import BigNumber ( BigNumber, bnZero, bnOne, bnTwo, sumBN, subBN, bnToInt )

{- FIB WITH INT -}
fibRec :: (Integral a) => a -> a -- Returns the nth Fibonacci number using a simple recursive approach
fibRec 0 = 0
fibRec 1 = 1
fibRec n = fibRec(n-2) + fibRec(n-1)

fibList :: Int -> Int -- Returns the nth Fibonacci number using a dynamic programming approach
fibList 0 = 0
fibList 1 = 1
fibList n = fl !! n
    where  fl = 0 : 1 : [fl !! (x-2) + fl !! (x-1) | x <- [2..n]]

fibListInf :: [Integer] -- Generates an infinite list of Fibonacci numbers and returns the nth Fibonacci number
fibListInf = 0 : 1 : [a+b | (a,b)<-zip fibListInf (tail fibListInf)]

fibListInfZip :: [Integer] -- Generates an infinite list of Fibonacci numbers and returns the nth Fibonacci number
fibListInfZip = 0 : 1 : zipWith (+) fibListInfZip (tail fibListInfZip)

fibMinMax :: Int -> Int -> [Integer]
fibMinMax nmin nmax = map (fibListInf!!) [nmin..nmax] -- Returns an interval of Fibonacci numbers between nmin and nmax


{- FIB WITH BN -}
fibRecBN :: Int -> BigNumber -- Returns the nth Fibonacci number as a BigNumber using a simple recursive approach
fibRecBN 0 = bnZero
fibRecBN 1 = bnOne
fibRecBN n = sumBN (fibRecBN (n-2)) (fibRecBN (n-1))

fibRecBNBN :: BigNumber -> BigNumber -- Returns the nth Fibonacci number as a BigNumber using a simple recursive approach. Takes a BigNumber as an argument
fibRecBNBN bn
    | bn == bnZero = bnZero 
    | bn == bnOne = bnOne
    | otherwise = sumBN (fibRecBNBN (subBN bn bnTwo)) (fibRecBNBN (subBN bn bnOne))

fibListBN :: Int -> BigNumber -- Returns the nth Fibonacci number as a BigNumber using a dynamic programming approach
fibListBN 0 = bnZero
fibListBN 1 = bnOne
fibListBN n = fl !! n
    where  fl = bnZero : bnOne : [sumBN (fl !! (x-2)) (fl !! (x-1)) | x <- [2..n]]

fibListBNBN :: BigNumber -> BigNumber {-- Returns the nth Fibonacci number as a BigNumber using a dynamic programming approach. Takes a BigNumber as an argument
instead of an Int --}
fibListBNBN bn 
    | bn == bnZero = bnZero 
    | bn == bnOne = bnOne 
    | otherwise = fl !! bnToInt bn
        where  fl = bnZero : bnOne : [sumBN (fl !! (x-2)) (fl !! (x-1)) | x <- [2..bnToInt bn]]

fibListInfBN :: [BigNumber] -- Generates an infinite list of Fibonacci numbers and returns the nth Fibonacci number as a BigNumber
fibListInfBN = bnZero : bnOne : [sumBN a b | (a,b)<-zip fibListInfBN (tail fibListInfBN)]

fibListInfZipBN :: [BigNumber] -- Generates an infinite list of Fibonacci numbers and returns the nth Fibonacci number as a BigNumber
fibListInfZipBN = bnZero : bnOne : zipWith sumBN fibListInfZipBN (tail fibListInfZipBN)

fibMinMaxBN :: Int -> Int -> [BigNumber]  -- Returns an interval of Fibonacci numbers between nmin and nmax as BigNumbers
fibMinMaxBN nmin nmax = map (fibListInfBN!!) [nmin..nmax]



--End