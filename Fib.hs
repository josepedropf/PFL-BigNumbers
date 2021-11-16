

fibLista :: [Integer]
fibLista = 0 : 1 : [a+b | (a,b)<-zip fibLista (tail fibLista)]

fibLMinMax :: Int -> Int -> [Integer]
fibLMinMax nmin nmax = map (fibLista!!) [nmin..nmax]
