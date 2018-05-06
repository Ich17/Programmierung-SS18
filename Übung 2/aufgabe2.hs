-- bei Fragen bitte an Andreas.Geyer@tu-dresden.de schreiben

-- die bereits vordefinierten Funktionen "verstecken"
import Prelude hiding(words, unwords)


-- a

unwords :: [String] -> String
unwords [] = ""
unwords [s] = s
unwords (w : ws) = w ++ " " ++ unwords ws


-- b

words :: String -> [String]
words [] = []
words ws = u : words us
  where
    (u, us) = breakAtSpace ws
    breakAtSpace [] = ([], [])
    breakAtSpace (' ' : cs) = ([], cs)
    breakAtSpace (c : cs) = let (v, vs) = breakAtSpace cs
                            in (c:v, vs)
