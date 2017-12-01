module KNN where

import Iris
import Data.List
import Data.Ord

type Distancia = Float -> Float -> Float
type Votacio = Float -> Float
type Avaluacio = String -> String -> Int

applyAv :: [Iris] -> [String] -> (Float, Float)
applyAv ts ps = ((avaluacio ps (map (irisClass) ts) accuracy),
                 (avaluacio ps (map (irisClass) ts) lost    ))

-- k, tests, entrenaments, distancia, votacio

irisKNN :: Int -> [Iris] -> [Iris] -> Distancia -> Char -> [String]
irisKNN k ts es d v = map (moreFrquent) (knn k ts es d v)

moreFrquent :: [String] -> String
moreFrquent ss = fst $ maximumBy (comparing snd) $ map (countFrequency ss) ss

countFrequency :: [String] -> String -> (String, Int)
countFrequency xs s = (s, length $ filter (== s) xs)

knn :: Int -> [Iris] -> [Iris] -> Distancia -> Char -> [[String]]
knn k ts es d v = map (kNearestNeighbors k d v es) ts

kNearestNeighbors :: Int -> Distancia -> Char -> [Iris] -> Iris -> [String]
kNearestNeighbors k d v es t
    | v == 'P'  = map (neighborClass) $ take k $ reverse $
                    sort $ map (neighbor d ponderada t) es
    | otherwise = map (neighborClass) $ take k $
                    sort $ map (neighbor d basica t) es

neighbor :: Distancia -> Votacio -> Iris -> Iris -> IrisNeighbor
neighbor d v x y = IrisNeighbor (irisClass y) (v $ sumaDistancies x y d)

sumaDistancies :: Iris -> Iris -> Distancia -> Float
sumaDistancies x y f = sumaDistancies' (array x) (array y) f

sumaDistancies' :: [Float] -> [Float] -> Distancia -> Float
sumaDistancies' xs ys f = sqrt $ sum $ zipWith f xs ys

euclediana :: Float -> Float -> Float
euclediana x y = (x - y)^2

manhattan :: Float -> Float -> Float
manhattan x y = abs $ x - y

basica :: Float -> Float
basica x = x

ponderada :: Float -> Float
ponderada x = 1.0 / x

avaluacio :: [String] -> [String] -> Avaluacio -> Float
avaluacio _ [] _ = 0.0
avaluacio p t a = (fromIntegral $ sum $ zipWith (a) p t) / (fromIntegral $ length t)

accuracy :: String -> String -> Int
accuracy s1 s2
    | s1 == s2  = 1
    | otherwise = 0

lost :: String -> String -> Int
lost s1 s2
    | s1 == s2  = 0
    | otherwise = 1











