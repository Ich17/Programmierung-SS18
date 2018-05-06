-- bei Fragen bitte an Andreas.Geyer@tu-dresden.de schreiben

max_length :: [[Int]] -> Int
max_length [] = -1
max_length (x:xs) = max2 (laenge x) (max_length xs)


laenge :: [Int] -> Int
laenge [] = 0
laenge (x:xs) = 1 + (laenge xs)


max2 :: Int -> Int -> Int
max2 x y
  | x >= y = x
  | otherwise = y
