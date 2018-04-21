-- bei Fragen bitte an Andreas.Geyer@tu-dresden.de schreiben

-- a

fac :: Int -> Int
fac 0 = 1
fac n = n * fac (n-1)


-- a besser
-- prdouct ist eine bereits in der Standardbibliothek vorhandene Funktion

fac' :: Int -> Int
fac' n = product [1 .. n]


-- b

sumFacs :: Int -> Int -> Int
sumFacs n m
      | m < n = 0
      | otherwise = fac n + sumFacs (n + 1) m


-- b besser
-- sum ist eine bereits in der Standardbibliothek vorhandene Funktion

sumFacs' :: Int -> Int -> Int
sumFacs' n m = sum [fac' i | i <- [n .. m]]
