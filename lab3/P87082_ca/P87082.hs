indexMassaCorporal :: String -> String
indexMassaCorporal line = (head $ words line) ++ ": " ++ imc m h
    where (m:h:_) = map read $ tail $ words line :: [Float]

imc :: Float -> Float -> String
imc m h = imc' (m / (h^2))

imc' :: Float -> String
imc' i
    | i < 18    = "magror"
    | i < 25    = "corpulencia normal"
    | i < 30    = "sobrepes"
    | i < 40    = "obesitat"
    | otherwise = "obesitat morbida"

main = do
    line <- getLine
    if (line /= "*") then
        do  putStrLn $ indexMassaCorporal line
            main
    else
        return ()