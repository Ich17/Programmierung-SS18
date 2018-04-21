-- bei Fragen bitte an Andreas.Geyer@tu-dresden.de schreiben


fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib i = fib (i-1) + fib (i-2)


-- besser (in linearer Zeit) -> dazu mal zusatzaufgabe1.hs anschauen und testen
-- hier sind nicht f0 und f1 sondern f1 und f2 als 1 definiert

fib' :: Int -> Int
fib' = fib'' 0 1

fib'' :: Int -> Int -> Int -> Int
fib'' x _ 0 = x
fib'' x y n = fib'' y (x + y) (n - 1)
