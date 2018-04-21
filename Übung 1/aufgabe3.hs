-- bei Fragen bitte an Andreas.Geyer@tu-dresden.de schreiben

-- bereits in der Standarbibliothek implementierte Funktionen ausblenden
import Prelude hiding (rem)


-- a

prod :: [Int] -> Int
prod [] = 1
prod (x:xs) = x * prod xs


-- b

rev :: [Int] -> [Int]
rev [] = []
rev (x:xs) = rev xs ++ [x]


-- b besser

rev' :: [Int] -> [Int]
rev' xs = aux xs []
  where
    aux [] ys = ys
    aux (x : xs) ys = aux xs (x:ys)


-- c

rem :: Int -> [Int] -> [Int]
rem _ [] = []
rem x (y:ys)
  | x == y = rem x ys
  | otherwise = y : rem x ys


-- d

isOrd :: [Int] -> Bool
isOrd [] = True
isOrd [x] = True
isOrd (x : y : xs) = x <= y && isOrd (y:xs)


-- e

merge :: [Int] -> [Int] -> [Int]
merge [] ys = ys
merge xs [] = xs
merge (x : xs) (y : ys)
  | x < y = x : merge xs (y:ys)
  | otherwise = y : merge (x : xs) ys
