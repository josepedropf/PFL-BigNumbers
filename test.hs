import BigNumber ( BigNumber,output, scanner, bnZero, bnOne, bnTwo, sumBN, subBN, mulBN, divBN, bnToInt )
import Fib (fibRec, fibList, fibListInf, fibRecBNBN, fibListBNBN, fibListInfBN)
-- Testing scanner and output is kinda weird and redundant with IO ()

{- ----------------------------Testing Fib.hs functions------------------------------- -}

testFibRec :: IO ()
testFibRec = do
    putStr "This function will return the nth Fibonacci number, n = "
    n <- getLine
    putStrLn (show (fibRec (read n :: Int)))

testFibList :: IO ()
testFibList = do
    putStr "This function will return the nth Fibonacci number, n = "
    n <- getLine
    putStrLn (show (fibList (read n :: Int)))

testFibListInf :: IO ()
testFibListInf = do
    putStr "This function will return the nth Fibonacci number, n = "
    n <- getLine
    putStrLn (show (fibListInf !! (read n :: Int)))

testFibRecBNBN :: IO ()
testFibRecBNBN = do
    putStr "This function will return the nth Fibonacci number using the BigNumber library, n = "
    n <- getLine
    putStrLn (output(fibRecBNBN (scanner n)))

testFibListBNBN :: IO ()
testFibListBNBN = do
    putStr "This function will return the nth Fibonacci number using the BigNumber library, n = "
    n <- getLine
    putStrLn (output(fibListBNBN (scanner n)))

testFibListInfBN :: IO ()
testFibListInfBN = do
    putStr "This function will return the nth Fibonacci number using the BigNumber library, n = "
    n <- getLine
    putStrLn (output(fibListInfBN !! (read n :: Int)))

{- ----------------------------Testing BigNumber.hs functions------------------------------- -}

testSomaBN :: IO ()
testSomaBN = do
    putStr "Input the first number: "
    firstn <- getLine
    putStr "Input the second number: "
    secondn <- getLine
    putStrLn (output(sumBN(scanner firstn) (scanner secondn)))

testSubBN :: IO () 
testSubBN = do
    putStr "Input the first number: "
    firstn <- getLine
    putStr "Input the second number: "
    secondn <- getLine
    putStrLn (output(subBN(scanner firstn) (scanner secondn)))

testMulBN :: IO ()
testMulBN = do
    putStr "Input the first number: "
    firstn <- getLine
    putStr "Input the second number: "
    secondn <- getLine
    putStrLn (output(mulBN(scanner firstn) (scanner secondn)))
{-
testDivBN :: IO ()  
testDivBN = do
    putStr "Input the first number: "
    firstn <- getLine
    putStr "Input the second number: "
    secondn <- getLine
    putStrLn (output(fst (divBN (scanner firstn) (scanner secondn)))) 
-}