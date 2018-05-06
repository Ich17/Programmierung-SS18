-- bei Fragen bitte an Andreas.Geyer@tu-dresden.de schreiben

f :: [Int] -> Int
f xs = foldr (*) 1 (map (\x -> x * x ) (filter even xs))
