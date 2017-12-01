module Iris where

data Sepal = Sepal Float Float deriving (Show)
data Petal = Petal Float Float deriving (Show)
data Iris = Iris String Sepal Petal deriving (Show)

irisClass :: Iris -> String
irisClass (Iris c _ _) = c

sepalLength :: Iris -> Float
sepalLength (Iris _ (Sepal sl _) _) = sl

sepalWidth :: Iris -> Float
sepalWidth (Iris _ (Sepal _ sw) _) = sw

petalLength :: Iris -> Float
petalLength (Iris _ _ (Petal pl _)) = pl

petalWidth :: Iris -> Float
petalWidth (Iris _ _ (Petal _ pw)) = pw

array :: Iris -> [Float]
array (Iris _ (Sepal sl sw) (Petal pl pw)) = [sl, sw, pl, pw]

data IrisNeighbor = IrisNeighbor String Float deriving (Show)

neighborClass :: IrisNeighbor -> String
neighborClass (IrisNeighbor c d) = c

instance Eq IrisNeighbor where
    (IrisNeighbor c1 d1) == (IrisNeighbor c2 d2) = c1 == c2 && d1 == d2

instance Ord IrisNeighbor where
    compare (IrisNeighbor _ d1) (IrisNeighbor _ d2) = compare d1 d2

instance Eq Iris where
    (Iris c1 (Sepal sl1 sw1) (Petal pl1 pw1)) == (Iris c2 (Sepal sl2 sw2) (Petal pl2 pw2)) = c1 == c2 && sl1 == sl2 && sw1 == sw2 && pl1 == pl2 && pw1 == pw2