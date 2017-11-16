saluda :: String -> String
saluda x
    | last x == 'A' || last x == 'a' = "Hola maca!"
    | otherwise                      = "Hola maco!"

main = do
    nom <- getLine
    let salut = saluda nom
    putStrLn salut