module Reader where

import Data.List.Split
import Iris
import KNN

parseI :: String -> [Iris]
parseI text = map (arrayToIris . lineToArray) (textToLines text)

textToLines :: String -> [String]
textToLines text = splitOn "\n" text

lineToArray :: String -> [String]
lineToArray line = splitOn "," line

arrayToIris :: [String] -> Iris
arrayToIris (sl:(sw:(pl:(pw:c)))) = Iris (last c)
                                         (Sepal (read sl) (read sw))
                                         (Petal (read pl) (read pw))

parseK :: String -> Int
parseK k = (read k)

parseD :: String -> Distancia
parseD d
    | d == "M"  = manhattan
    | otherwise = euclediana

parseV :: String -> Char
parseV v
    | v == "P"  = 'P'
    | otherwise = 'B'

welcome = "Benvingut al Iris k Nearest Neighbors de Fèlix Arribas"

bye = "\n\nGràcies. Pots veure el codi a https://github.com/felixarpa/LP-Haskell/tree/master/practica"

kMessage :: Int -> String
kMessage maxim = "\nVariable k (entre 1 i " ++ (show maxim) ++ "): "

dMessage = "\nFunció de distancia. Euclediana (E) o Manhattan (M): "
dValues = [ "E", "M" ]
vMessage = "\nMecanisme de votació: Simple (S) o Ponderat (P): "
vValues = [ "S", "P" ]

aMessage = "\nAvaluació accuracy o exactitud: "
lMessage = "\nAvaluació lost o error:         "

get :: String -> [String] -> IO String
get message correctValues = do    
    putStrLn message
    line <- getLine
    if elem line correctValues then
        return(line)
    else
        do get message correctValues

