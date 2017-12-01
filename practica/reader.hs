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

