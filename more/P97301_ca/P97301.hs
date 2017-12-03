fizzBuzz :: [Either Int String]
fizzBuzz = map substitueix $ map (Left) $ iterate (+1) 0

substitueix :: Either Int String -> Either Int String
substitueix (Left x)
    | x `mod` 3 == 0 && x `mod` 5 == 0 = Right "FizzBuzz"
    | x `mod` 3 == 0 = Right "Fizz"
    | x `mod` 5 == 0 = Right "Buzz"
    | otherwise = Left x