import Data.List

serieCollatz :: Integer -> [Integer]
serieCollatz n = takeWhile (/= 0) $ iterate (iterateCollatz) n

iterateCollatz :: Integer -> Integer
iterateCollatz n
    | n == 1       = 0
    | mod n 2 == 0 = div n 2
    | mod n 2 == 1 = n * 3 + 1

collatzMesLlarga :: Integer -> Integer
collatzMesLlarga n = maximum $ collatzLengths [1..n]

collatzLengths :: [Integer] -> [Integer]
collatzLengths xs = map (toInteger . length) $ map (serieCollatz) xs

representantsCollatz :: [Integer] -> [Integer]
representantsCollatz xs = map fst $ foldl putRepresentant [] $ zipWith (,) xs $ collatzLengths xs

putRepresentant :: [(Integer, Integer)] -> (Integer, Integer) -> [(Integer, Integer)]
putRepresentant xs x
    | elem (snd x) (map (snd) xs) = xs
    | otherwise = takeWhile (greaterOrEqual x) xs ++ [x] ++ dropWhile (greaterOrEqual x) xs

greaterOrEqual :: (Integer, Integer) -> (Integer, Integer) -> Bool
greaterOrEqual x y = snd x >= snd y

classeCollatz :: Integer -> Either Int [Integer]
classeCollatz x
    | 35 < (length $ serieCollatz x) = Left (length $ serieCollatz x)
    | otherwise = Right (serieCollatz x)

--classeCollatz' :: Integer -> [Integer]
