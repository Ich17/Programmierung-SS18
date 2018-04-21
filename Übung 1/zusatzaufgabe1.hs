-- bei Fragen bitte an Andreas.Geyer@tu-dresden.de schreiben

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib i = fib (i-1) + fib (i-2)

fibs :: [Int]
fibs = f' 0
  where
    f' i = fib i : f' (i + 1)


-- zum Vergleich der Geschwindigkeiten von fib und fib' :

fib' :: Int -> Int
fib' = fib'' 0 1

fib'' :: Int -> Int -> Int -> Int
fib'' x _ 0 = x
fib'' x y n = fib'' y (x + y) (n - 1)

fibs' :: [Int]
fibs' = f'' 0
  where
    f'' i = fib' i : f'' (i + 1)
