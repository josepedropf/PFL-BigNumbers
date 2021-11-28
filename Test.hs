import BigNumber ( BigNumber,output, scanner, bnZero, bnOne, bnTwo, somaBN, subBN, mulBN, divBN, bnToInt, rawDivBN, absBN, safeDivBN)
import Fib (fibRec, fibLista, fibListaInfinita, fibRecIntBN, fibRecBN, fibListaBN, fibListaInfinitaBN, fibListaIntBN)
-- Testing scanner and output is kinda weird and redundant with IO ()

{- Auxiliary Functions -}
outputTuple :: (BigNumber, BigNumber) -> String
outputTuple tup = "(" ++ output (fst tup) ++ ", " ++ output (snd tup) ++ ")"

outputMaybeBN :: Maybe BigNumber -> String
outputMaybeBN = maybe "There is no BN" output

outputMaybeTuple :: Maybe (BigNumber, BigNumber) -> String -> String
outputMaybeTuple mtp nothing_msg
    | mtp == Nothing = nothing_msg
    | otherwise = "(" ++ outputMaybeBN (fst <$> mtp) ++ ", " ++ outputMaybeBN (snd <$> mtp) ++ ")"


{- Testing Fib.hs functions -}

testFibRec :: IO () -- Helper funtion to make testing fibRec easier
testFibRec = do
    putStr "This function will return the nth Fibonacci number, n = "
    n <- getLine
    print (fibRec (read n :: Int))

testFibLista :: IO () -- Helper funtion to make testing fibLista easier
testFibLista = do
    putStr "This function will return the nth Fibonacci number, n = "
    n <- getLine
    print (fibLista (read n :: Int))

testFibListaInfinita :: IO () -- Helper funtion to make testing fibListaInfinita easier
testFibListaInfinita = do
    putStr "This function will return the nth Fibonacci number, n = "
    n <- getLine
    print (fibListaInfinita !! (read n :: Int))

testFibRecIntBN :: IO () -- Helper funtion to make testing fibRecBN easier
testFibRecIntBN = do
    putStr "This function will return the nth Fibonacci number using the BigNumber library, n = "
    n <- getLine
    putStrLn (output(fibRecIntBN (read n :: Int)))

testFibRecBN :: IO () -- Helper funtion to make testing fibRecBNBN easier
testFibRecBN = do
    putStr "This function will return the nth Fibonacci number using the BigNumber library, n = "
    n <- getLine
    putStrLn (output(fibRecBN (scanner n)))

testFibListaBN :: IO () -- Helper funtion to make testing fibListaBNBN easier
testFibListaBN = do
    putStr "This function will return the nth Fibonacci number using the BigNumber library, n = "
    n <- getLine
    putStrLn (output(fibListaBN (scanner n)))

testFibListaIntBN :: IO () -- Helper funtion to make testing fibListaIntBN easier
testFibListaIntBN = do
    putStr "This function will return the nth Fibonacci number using the BigNumber library, n = "
    n <- getLine
    putStrLn (output(fibListaIntBN (read n :: Int)))    

testFibListaInfinitaBN :: IO () -- Helper funtion to make testing fibListaInfinitaBN easier
testFibListaInfinitaBN = do
    putStr "This function will return the nth Fibonacci number using the BigNumber library, n = "
    n <- getLine
    putStrLn (output(fibListaInfinitaBN !! (read n :: Int)))


{- Testing BigNumber.hs functions -}

testSomaBN :: IO () -- Helper funtion to make testing somaBN easier
testSomaBN = do
    putStr "Input the first number: "
    firstn <- getLine
    putStr "Input the second number: "
    secondn <- getLine
    putStrLn (output(somaBN(scanner firstn) (scanner secondn)))

testSubBN :: IO () -- Helper funtion to make testing subBN easier
testSubBN = do
    putStr "Input the first number: "
    firstn <- getLine
    putStr "Input the second number: "
    secondn <- getLine
    putStrLn (output(subBN(scanner firstn) (scanner secondn)))

testMulBN :: IO () -- Helper funtion to make testing mulBN easier
testMulBN = do
    putStr "Input the first number: "
    firstn <- getLine
    putStr "Input the second number: "
    secondn <- getLine
    putStrLn (output(mulBN(scanner firstn) (scanner secondn)))

testDivBN :: IO () -- Divides two BigNumbers using the rawDivBN function
testDivBN = do
    putStr "Input the first number: "
    firstn <- getLine
    putStr "Input the second number: "
    secondn <- getLine
    putStrLn (outputTuple (divBN (scanner firstn) (scanner secondn)))

testSafeDivBN :: IO () -- Divides two BigNumbers using the rawDivBN function
testSafeDivBN = do
    putStr "Input the first number: "
    firstn <- getLine
    putStr "Input the second number: "
    secondn <- getLine
    putStrLn (outputMaybeTuple (safeDivBN (scanner firstn) (scanner secondn)) "Math Error -> Division by Zero")

--End