sumMultiples35 :: Integer -> Integer
sumMultiples35 n = sum $ filter (multiple35) [0..n - 1]

multiple35 :: Integer -> Bool
multiple35 x = x `mod` 3 == 0 ||
               x `mod` 5 == 0

fibonacci :: Int -> Integer
fibonacci n = last $ take (n + 1) $ fibonaccis 0 1

fibonaccis :: Integer -> Integer -> [Integer]
fibonaccis x y = x : fibonaccis y (x + y)

sumEvenFibonaccis :: Int -> Integer
sumEvenFibonaccis n = sum $ filter (even) $ take (n - 1) $ fibonaccis 0 1

largestPrimeFactor :: Integer -> Integer
largestPrimeFactor n = last $ filter (isPrime) $ filter (isFactor n) [1..n]

isFactor :: Integer -> Integer -> Bool
isFactor x y = x `mod` y == 0 

isPrimeRec :: Integer -> Integer -> Bool
isPrimeRec x div
    | div == 1 = True
    | x `mod` div == 0 = False
    | otherwise = isPrimeRec x (div - 1)

isPrime :: Integer -> Bool
isPrime x
    | x == 0 = False
    | x == 1 = False
    | otherwise = isPrimeRec x (floor (sqrt (fromIntegral x)))

isPalindromic :: Integer -> Bool
isPalindromic n = and $ zipWith (==) (show n) (reverse $ show n)
