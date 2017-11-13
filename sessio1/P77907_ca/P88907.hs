absValue :: Integer -> Integer
absValue x
    | x < 0 = - x
    | otherwise = x


power :: Integer -> Integer -> Integer
power base exponent
    | exponent == 0 = 1
    | otherwise = base * (power base (exponent - 1))


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

-- fromIntegral fa un cast
-- floor agafa la part entera d'un Float

slowFib :: Integer -> Integer
slowFib 0 = 0
slowFib 1 = 1
slowFib n = slowFib (n - 1) + slowFib (n -2)



quickFib :: Integer -> Integer
quickFib n
    | n == 0 = 0
    | n == 1 = 1
    | otherwise = quickFibRec 0 1 2
    where
        quickFibRec :: Integer -> Integer -> Integer -> Integer
        quickFibRec first second it
            | it == n = first + second
            | otherwise = quickFibRec second (first + second) (it + 1)

