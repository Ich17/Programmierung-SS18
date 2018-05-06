-- bei Fragen bitte an Andreas.Geyer@tu-dresden.de schreiben

data Tree a = Branch a (Tree a) (Tree a) | Leaf a deriving Show

-- a
-- Beispielbaum t definieren

t :: Tree Int
t = Branch 1  (Branch 2 (Leaf 3) (Leaf 4))
              (Branch 5 (Branch 6 (Leaf 7) (Leaf 8)) (Leaf 9))


-- b

depth :: Tree a -> Int
depth (Leaf _) = 1
depth (Branch _ l r) = 1 + (min (depth l) (depth r))


-- c

paths :: Tree a -> Tree [a]
paths = paths' []
  where
    paths' p (Leaf a) = Leaf (p ++ [a])
    paths' p (Branch a l r) = let p' = p ++ [a]
                              in Branch p' (paths' p' l) (paths' p' r)


-- d

tmap :: (a -> b) -> Tree a -> Tree b
tmap f (Leaf a) = Leaf (f a)
tmap f (Branch a l r) = Branch (f a) (tmap f l) (tmap f r)
