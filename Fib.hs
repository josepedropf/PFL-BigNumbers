module Fib (fibRec, fibRecBN, fibRecIntBN, fibLista, fibListaInfinita, fibListaBN, fibListaInfinitaBN, fibListaIntBN) where 
import BigNumber ( BigNumber, bnZero, bnOne, bnTwo, somaBN, subBN, indexBN, bnToInt )

{- FIB WITH INT -}
fibRec :: (Integral a) => a -> a -- Returns the nth Fibonacci number using a simple recursive approach
fibRec 0 = 0
fibRec 1 = 1
fibRec n = fibRec(n-2) + fibRec(n-1)

fibLista :: (Integral a) => Int -> a -- Returns the nth Fibonacci number using a dynamic programming approach
fibLista 0 = 0
fibLista 1 = 1
fibLista n = fl !! n
    where  fl = 0 : 1 : [fl !! (x-2) + fl !! (x-1) | x <- [2..n]]

fibListaInfinita :: [Integer] -- Generates an infinite list of Fibonacci numbers and returns the nth Fibonacci number
fibListaInfinita = 0 : 1 : zipWith (+) fibListaInfinita (tail fibListaInfinita)

fibMinMax :: Int -> Int -> [Integer]
fibMinMax nmin nmax = map (fibListaInfinita!!) [nmin..nmax] -- Returns an interval of Fibonacci numbers between nmin and nmax


{- FIB WITH BN -}
fibRecIntBN :: Int -> BigNumber -- Returns the nth Fibonacci number as a BigNumber using a simple recursive approach
fibRecIntBN 0 = bnZero
fibRecIntBN 1 = bnOne
fibRecIntBN n = somaBN (fibRecIntBN (n-2)) (fibRecIntBN (n-1))

fibRecBN :: BigNumber -> BigNumber -- Returns the nth Fibonacci number as a BigNumber using a simple recursive approach. Takes a BigNumber as an argument
fibRecBN bn
    | bn == bnZero = bnZero 
    | bn == bnOne = bnOne
    | otherwise = somaBN (fibRecBN (subBN bn bnTwo)) (fibRecBN (subBN bn bnOne))

fibListaIntBN :: Int -> BigNumber -- Returns the nth Fibonacci number as a BigNumber using a dynamic programming approach
fibListaIntBN 0 = bnZero
fibListaIntBN 1 = bnOne
fibListaIntBN n = fl !! n
    where  fl = bnZero : bnOne : [somaBN (fl !! (x-2)) (fl !! (x-1)) | x <- [2..n]]

fibListaBN :: BigNumber -> BigNumber {-- Returns the nth Fibonacci number as a BigNumber using a dynamic programming approach. Takes a BigNumber as an argument
instead of an Int --}
fibListaBN bn 
    | bn == bnZero = bnZero 
    | bn == bnOne = bnOne 
    | otherwise = indexBN fl bn
        where  fl = bnZero : bnOne : [somaBN (fl !! (x-2)) (fl !! (x-1)) | x <- [2..bnToInt bn]]

fibListaInfinitaBN :: [BigNumber] -- Generates an infinite list of Fibonacci numbers and returns the nth Fibonacci number as a BigNumber
fibListaInfinitaBN = bnZero : bnOne : zipWith somaBN fibListaInfinitaBN (tail fibListaInfinitaBN)

fibMinMaxBN :: Int -> Int -> [BigNumber]  -- Returns an interval of Fibonacci numbers between nmin and nmax as BigNumbers
fibMinMaxBN nmin nmax = map (fibListaInfinitaBN!!) [nmin..nmax]

--End