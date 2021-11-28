module Fib (fibRec, fibRecBN, fibLista, fibListaInfinita, fibRecBNBN, fibListaBNBN, fibListaInfinitaBN) where 
import BigNumber ( BigNumber, bnZero, bnOne, bnTwo, somaBN, subBN, bnToInt )

{- FIB WITH INT -}
fibRec :: (Integral a) => a -> a -- Returns the nth Fibonacci number using a simple recursive approach
fibRec 0 = 0
fibRec 1 = 1
fibRec n = fibRec(n-2) + fibRec(n-1)

fibLista :: Int -> Int -- Returns the nth Fibonacci number using a dynamic programming approach
fibLista 0 = 0
fibLista 1 = 1
fibLista n = fl !! n
    where  fl = 0 : 1 : [fl !! (x-2) + fl !! (x-1) | x <- [2..n]]

oldFibListaInfinita :: [Integer] -- Generates an infinite list of Fibonacci numbers and returns the nth Fibonacci number
oldFibListaInfinita = 0 : 1 : [a+b | (a,b)<-zip oldFibListaInfinita (tail oldFibListaInfinita)]

fibListaInfinita :: [Integer] -- Generates an infinite list of Fibonacci numbers and returns the nth Fibonacci number
fibListaInfinita = 0 : 1 : zipWith (+) fibListaInfinita (tail fibListaInfinita)

fibMinMax :: Int -> Int -> [Integer]
fibMinMax nmin nmax = map (fibListaInfinita!!) [nmin..nmax] -- Returns an interval of Fibonacci numbers between nmin and nmax


{- FIB WITH BN -}
fibRecBN :: Int -> BigNumber -- Returns the nth Fibonacci number as a BigNumber using a simple recursive approach
fibRecBN 0 = bnZero
fibRecBN 1 = bnOne
fibRecBN n = somaBN (fibRecBN (n-2)) (fibRecBN (n-1))

fibRecBNBN :: BigNumber -> BigNumber -- Returns the nth Fibonacci number as a BigNumber using a simple recursive approach. Takes a BigNumber as an argument
fibRecBNBN bn
    | bn == bnZero = bnZero 
    | bn == bnOne = bnOne
    | otherwise = somaBN (fibRecBNBN (subBN bn bnTwo)) (fibRecBNBN (subBN bn bnOne))

fibListaBN :: Int -> BigNumber -- Returns the nth Fibonacci number as a BigNumber using a dynamic programming approach
fibListaBN 0 = bnZero
fibListaBN 1 = bnOne
fibListaBN n = fl !! n
    where  fl = bnZero : bnOne : [somaBN (fl !! (x-2)) (fl !! (x-1)) | x <- [2..n]]

fibListaBNBN :: BigNumber -> BigNumber {-- Returns the nth Fibonacci number as a BigNumber using a dynamic programming approach. Takes a BigNumber as an argument
instead of an Int --}
fibListaBNBN bn 
    | bn == bnZero = bnZero 
    | bn == bnOne = bnOne 
    | otherwise = fl !! bnToInt bn
        where  fl = bnZero : bnOne : [somaBN (fl !! (x-2)) (fl !! (x-1)) | x <- [2..bnToInt bn]]

oldFibListaInfinitaBN :: [BigNumber] -- Generates an infinite list of Fibonacci numbers and returns the nth Fibonacci number as a BigNumber
oldFibListaInfinitaBN = bnZero : bnOne : [somaBN a b | (a,b)<-zip oldFibListaInfinitaBN (tail oldFibListaInfinitaBN)]

fibListaInfinitaBN :: [BigNumber] -- Generates an infinite list of Fibonacci numbers and returns the nth Fibonacci number as a BigNumber
fibListaInfinitaBN = bnZero : bnOne : zipWith somaBN fibListaInfinitaBN (tail fibListaInfinitaBN)

fibMinMaxBN :: Int -> Int -> [BigNumber]  -- Returns an interval of Fibonacci numbers between nmin and nmax as BigNumbers
fibMinMaxBN nmin nmax = map (fibListaInfinitaBN!!) [nmin..nmax]



--End