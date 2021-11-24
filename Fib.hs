import BigNumber ( BigNumber, bnZero, bnOne, bnTwo, sumBN, subBN )

{--
fibListaInfinta n = infFib!!n
infFib = 0 : 1 : zipWith (+) infFib (tail infFib)
--}

--Integer--

fibRec :: (Integral a) => a -> a
fibRec 0 = 0
fibRec 1 = 1
fibRec n = fibRec(n-2) + fibRec(n-1)

fibList :: Int -> Int
fibList 0 = 0
fibList 1 = 1
fibList n = fl !! n
    where  fl = 0 : 1 : [fl !! (x-2) + fl !! (x-1) | x <- [2..n]]

fibListInf :: [Integer]
fibListInf = 0 : 1 : [a+b | (a,b)<-zip fibListInf (tail fibListInf)]

fibListInfZip :: [Integer]
fibListInfZip = 0 : 1 : zipWith (+) fibListInfZip (tail fibListInfZip)

fibMinMax :: Int -> Int -> [Integer]
fibMinMax nmin nmax = map (fibListInf!!) [nmin..nmax]


-- BigNumbers --


fibRecBN :: Integer -> BigNumber
fibRecBN 0 = bnZero
fibRecBN 1 = bnOne
fibRecBN n = sumBN (fibRecBN (n-2)) (fibRecBN (n-1))

fibListBN :: Int -> BigNumber 
fibListBN 0 = bnZero
fibListBN 1 = bnOne
fibListBN n = fl !! n
    where  fl = bnZero : bnOne : [sumBN (fl !! (x-2)) (fl !! (x-1)) | x <- [2..n]]

fibListInfBN :: [BigNumber]
fibListInfBN = bnZero : bnOne : [sumBN a b | (a,b)<-zip fibListInfBN (tail fibListInfBN)]

fibListInfZipBN :: [BigNumber]
fibListInfZipBN = bnZero : bnOne : zipWith sumBN fibListInfZipBN (tail fibListInfZipBN)

fibMinMaxBN :: Int -> Int -> [BigNumber]
fibMinMaxBN nmin nmax = map (fibListInfBN!!) [nmin..nmax]

