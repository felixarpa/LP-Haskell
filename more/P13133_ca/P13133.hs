sumMultiples35 :: Integer -> Integer
sumMultiples35 n = (sumMultiples 3 n) + (sumMultiples 5 n) - (sumMultiples 15 n)

sumMultiples :: Integer -> Integer -> Integer
sumMultiples x n = x * n' * (n' + 1) `div` 2
    where n' = div (n - 1) x

fibonacci :: Int -> Integer
fibonacci n = last $ take (n + 1) $ fibonaccis 0 1

fibonaccis :: Integer -> Integer -> [Integer]
fibonaccis x y = x : fibonaccis y (x + y)

sumEvenFibonaccis :: Integer -> Integer
sumEvenFibonaccis n = sum $ filter even $ takeWhile (< n) $ fibonaccis 0 1

largestPrimeFactor :: Integer -> Integer
largestPrimeFactor n = head $ filter (isPrime) $ filter (isFactor n) $ alReves n

alReves :: Integer -> [Integer]
alReves n = takeWhile (>0) $ iterate (sub) (div n 2)

sub :: Integer -> Integer
sub x = x - 1

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
