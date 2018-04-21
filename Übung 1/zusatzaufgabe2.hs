-- bei Fragen bitte an Andreas.Geyer@tu-dresden.de schreiben

countBinTrees :: Int -> Int
countBinTrees 0 = 0
countBinTrees 1 = 1
countBinTrees n = go (n - 1)
  where
    go 0 = 0
    go m = go (m - 1) + countBinTrees (n - 1 - m) * countBinTrees m
