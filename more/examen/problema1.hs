shuffleOnce :: [a] -> [a]
shuffleOnce x
    | even $ length x = shuffle x
    | otherwise = shuffle x ++ [last x]

shuffle :: [a] -> [a]
shuffle x = concat $ zipWith (mix) (primeraMeitat x) (segonaMeitat x)

mix :: a -> a -> [a]
mix x y = [y, x]

primeraMeitat :: [a] -> [a]
primeraMeitat x = take (length x `div` 2) x

segonaMeitat :: [a] -> [a]
segonaMeitat x = drop (length x `div` 2) x

shuffleBack :: Eq a => [a] -> Int
shuffleBack as = succ $ length $ takeWhile (/= as) $ tail $ iterate (shuffleOnce) as
-- succ = +1

segments :: Ord a => [a] -> [[a]]
