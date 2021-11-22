{--
fibRec :: (Integral a) => a -> a
fibRec 0 = 0
fibRec 1 = 1
fibRec n = fibRec(n-2) + fibRec(n-1)

fibListaInfinta n = infFib!!n
infFib = 0 : 1 : zipWith (+) infFib (tail infFib)
--}

fibList :: [Integer]
fibList = 0 : 1 : [a+b | (a,b)<-zip fibList (tail fibList)]

fibListInf :: [Integer]
fibListInf = 0 : 1 : (zipWith (+) fibListInf (tail fibListInf))

fibLMinMax :: Int -> Int -> [Integer]
fibLMinMax nmin nmax = map (fibListInf!!) [nmin..nmax]
