import Test.HUnit
import KNN
import Iris

-- https://hackage.haskell.org/package/HUnit
-- runTestTT tests

tests = TestList [  givenAppearingWord_whenCountFrequency_thenCountsCorrectly,
                    givenUnknownWord_whenCountFrequency_thenDoesNotCount,
                    givenEmptyArray_whenCountFrequency_thenDoesNotCount,
                    
                    givenWords_whenMoreFrequent_thenMostAppearing,
                    
                    givenFloat_whenPonderada_thenInversa,
                    
                    givenFloat_whenBasica_thenReturnSameValue,
                    
                    givenDifferentWords_whenValueAccuracy_thenReturnZero,
                    givenEqualWords_whenValueAccuracy_thenReturnOne,
                    
                    givenDifferentWords_whenValueLost_thenReturnOne,
                    givenEqualWords_whenValueLost_thenReturnZero,
                    
                    givenAccuarcyEval_whenComputeEvaluation_thenReturn25,
                    givenLostEval_whenComputeEvaluation_thenReturn75,
                    givenEmptyTestArray_whenComputeEvaluation_thenReturn0,
                    
                    givenTwoInputs_whenComputeManhattan_thenReturnValue,
                    givenTwoNegativeInputs_whenComputeManhattan_thenReturnValue,
                    
                    givenTwoInputs_whenComputeEuclediana_thenReturnValue,
                    givenTwoNegativeInputs_whenComputeEuclediana_thenReturnValue,
                    
                    givenFloatArrays_whenAddDistancesEuclediana_thenReturn,
                    givenFloatArrays_whenAddDistancesManhattan_thenReturn,
                    
                    givenIris_whenAddDistancesEuclediana_thenReturn,
                    givenIris_whenAddDistancesManhattan_thenReturn,
                    
                    givenIrisEucledianaBasica_whenNeighbor_thenIrisNeighbor,
                    givenIrisManhattanBasica_whenNeighbor_thenIrisNeighbor,
                    givenIrisEucledianaPonderada_whenNeighbor_thenIrisNeighbor,
                    givenIrisManhattanPonderada_whenNeighbor_thenIrisNeighbor,

                    givenK1Ponderada_whenKNeareastNeighbors_thenTomaquet,
                    givenK1Basica_whenKNeareastNeighbors_thenTomaquet,
                    givenK2Basica_whenKNeareastNeighbors_thenTomaquet,

                    givenK1NN_whenKNN_thenPatataCotxeTomaquet,
                    givenK2NN_whenKNN_thenPatatsTomaquetCotxeTomaquetTomaquetPatata,
                    givenK4NN_whenKNN_thenReturnWithRepetitions,

                    givenK3NN_whenIrisKNN_thenReturnMoreFrequent,
                    givenK1NN_whenIrisKNN_thenReturnMoreFrequent,

                    givenResults_whenApplyAv_thenPair ]

-- TEST LIST


-- countFrequency :: [String] -> String -> (String, Int)

input01a = ["Fèlix", "Arribas", "Pardo", "Fèlix", "Fèlix", "Pardo"]
input01b = "Pardo"
expected01 = ("Pardo", 2)
givenAppearingWord_whenCountFrequency_thenCountsCorrectly = 
    TestCase (assertEqual "Hauria de contar les aparicions de 'Pardo'"
              expected01
             (countFrequency input01a input01b))

input02a = ["Fèlix", "Arribas", "Pardo", "Fèlix", "Fèlix", "Pardo"]
input02b = "NO"
expected02 = ("NO", 0)
givenUnknownWord_whenCountFrequency_thenDoesNotCount =
    TestCase (assertEqual "Hauria de donar 0 aparicions de 'NO'"
              expected02
             (countFrequency input02a input02b))

input03a = []
input03b = "Felix"
expected03 = ("Felix", 0)
givenEmptyArray_whenCountFrequency_thenDoesNotCount =
    TestCase (assertEqual "Hauria de donar 0 aparicions de 'Felix'"
              expected03
             (countFrequency input03a input03b))


-- moreFrquent :: [String] -> String

input04 = ["Fèlix", "Arribas", "Pardo", "Fèlix", "Fèlix", "Pardo"]
expected04 = "Fèlix"
givenWords_whenMoreFrequent_thenMostAppearing =
    TestCase (assertEqual "La paraula que apareix més cops és Fèlix"
              expected04
             (moreFrquent input04))


-- ponderada :: Float -> Float

input05 = 2
expected05 = 0.5
givenFloat_whenPonderada_thenInversa =
    TestCase (assertEqual "La ponderació de 2 es 1/2 = 0.5"
              expected05
             (ponderada input05))

-- basica :: Float -> Float

input06 = 2
expected06 = 2
givenFloat_whenBasica_thenReturnSameValue =
    TestCase (assertEqual "La votació basica de 2 es 2"
              expected06
             (basica input06))


-- accuracy :: String -> String -> Int

input07a = "Paraula"
input07b = "Word"
expected07 = 0
givenDifferentWords_whenValueAccuracy_thenReturnZero =
    TestCase (assertEqual "Les paraules són diferents i hauria de donar 0"
              expected07
             (accuracy input07a input07b))

input08a = "Hola, bon dia"
input08b = "Hola, bon dia"
expected08 = 1
givenEqualWords_whenValueAccuracy_thenReturnOne =
    TestCase (assertEqual "Les paraules són iguals i hauria de donar 1"
              expected08
             (accuracy input08a input08b))


-- lost :: String -> String -> Int

input09a = "Paraula"
input09b = "Word"
expected09 = 1
givenDifferentWords_whenValueLost_thenReturnOne =
    TestCase (assertEqual "Les paraules són diferents i hauria de donar 1"
              expected09
             (lost input09a input09b))

input10a = "Hola, bon dia"
input10b = "Hola, bon dia"
expected10 = 0
givenEqualWords_whenValueLost_thenReturnZero =
    TestCase (assertEqual "Les paraules són iguals i hauria de donar 0"
              expected10
             (lost input10a input10b))


-- avaluacio :: [String] -> [String] -> Avaluacio -> Float

input11a = [ "Fèlix", "Arribas", "Pardo", "Bellaterra", "Fèlix", "Arribas", "Pardo", "Bellaterra" ]
input11b = [ "Arribas", "Pardo", "Fèlix", "Bellaterra", "Fèlix", "Bellaterra", "Arribas", "Pardo" ]
input11c = accuracy
expected11 = 0.25
givenAccuarcyEval_whenComputeEvaluation_thenReturn25 =
    TestCase (assertEqual "L'avaluacio accuracy de l'entrada és 0.25"
              expected11
             (avaluacio input11a input11b input11c))

input12a = [ "Fèlix", "Arribas", "Pardo", "Bellaterra", "Fèlix", "Arribas", "Pardo", "Bellaterra" ]
input12b = [ "Arribas", "Pardo", "Fèlix", "Bellaterra", "Fèlix", "Bellaterra", "Arribas", "Pardo" ]
input12c = lost
expected12 = 0.75
givenLostEval_whenComputeEvaluation_thenReturn75 =
    TestCase (assertEqual "L'avaluacio lost de l'entrada és 0.25"
              expected12
             (avaluacio input12a input12b input12c))

input13a = [ "Fèlix", "Arribas", "Pardo", "Bellaterra", "Fèlix", "Arribas", "Pardo", "Bellaterra" ]
input13b = []
input13c = lost
expected13 = 0.0
givenEmptyTestArray_whenComputeEvaluation_thenReturn0 =
    TestCase (assertEqual "L'avaluacio de l'entrada és 0.0"
              expected13
             (avaluacio input13a input13b input13c))


-- manhattan :: Float -> Float -> Float

input14a = 10.0
input14b = 2.0
expected14 = 8.0
givenTwoInputs_whenComputeManhattan_thenReturnValue =
    TestCase (assertEqual "|10 - 2| = 8"
              expected14
             (manhattan input14a input14b))

input15a = 9.0
input15b = 20.0
expected15 = 11.0
givenTwoNegativeInputs_whenComputeManhattan_thenReturnValue =
    TestCase (assertEqual "|9 - 20| = |-11| = 11"
              expected15
             (manhattan input15a input15b))


-- euclediana :: Float -> Float -> Float

input16a = 10.0
input16b = 2.0
expected16 = 64.0
givenTwoInputs_whenComputeEuclediana_thenReturnValue =
    TestCase (assertEqual "(10 - 2)^2 = 64"
              expected16
             (euclediana input16a input16b))

input17a = 9.0
input17b = 20.0
expected17 = 121.0
givenTwoNegativeInputs_whenComputeEuclediana_thenReturnValue =
    TestCase (assertEqual "(9 - 20)^2 = (-11)^2 = 121"
              expected17
             (euclediana input17a input17b))


-- sumaDistancies' :: [Float] -> [Float] -> Distancia -> Float

input18a = [ 1.0, 2.0, 3.0 ]
input18b = [ 2.0, 0.0, 1.0 ]
input18c = euclediana
expected18 = 3.0
givenFloatArrays_whenAddDistancesEuclediana_thenReturn =
    TestCase (assertEqual "La suma euclediana és 9, l'arrel 3"
              expected18
             (sumaDistancies' input18a input18b input18c))

input19a = [ 12.0, 2.0, 20.0 ]
input19b = [ 5.0, 10.0, 10.0 ]
input19c = manhattan
expected19 = 5.0
givenFloatArrays_whenAddDistancesManhattan_thenReturn =
    TestCase (assertEqual "La suma manhattan és 25, l'arrel 5"
              expected19
             (sumaDistancies' input19a input19b input19c))

-- sumaDistancies :: Iris -> Iris -> Distancia -> Float

input20a = Iris ""    (Sepal 1.0 2.0) (Petal 3.0 0.0)
input20b = Iris "Nom" (Sepal 2.0 0.0) (Petal 1.0 0.0)
input20c = euclediana
expected20 = 3.0
givenIris_whenAddDistancesEuclediana_thenReturn =
    TestCase (assertEqual "La suma euclediana és 9, l'arrel 3"
              expected20
             (sumaDistancies input20a input20b input20c))

input21a = Iris ""    (Sepal 12.0 2.0) (Petal 20.0 0.0)
input21b = Iris "Nom" (Sepal 5.0 10.0) (Petal 10.0 0.0)
input21c = manhattan
expected21 = 5.0
givenIris_whenAddDistancesManhattan_thenReturn =
    TestCase (assertEqual "La suma manhattan és 25, l'arrel 5"
              expected21
             (sumaDistancies input21a input21b input21c))

-- neighbor :: Distancia -> Votacio -> Iris -> Iris -> IrisNeighbor

input22a = euclediana
input22b = basica
input22c = Iris ""    (Sepal 1.0 2.0) (Petal 3.0 0.0)
input22d = Iris "Patata" (Sepal 2.0 0.0) (Petal 1.0 0.0)
expected22 = IrisNeighbor "Patata" 3.0
givenIrisEucledianaBasica_whenNeighbor_thenIrisNeighbor =
    TestCase (assertEqual "La suma euclediana és 9, l'arrel 3, el nom és 'Patata'"
              expected22
             (neighbor input22a input22b input22c input22d))

input23a = manhattan
input23b = basica
input23c = Iris ""    (Sepal 12.0 2.0) (Petal 20.0 0.0)
input23d = Iris "Patata" (Sepal 5.0 10.0) (Petal 10.0 0.0)
expected23 = IrisNeighbor "Patata" 5.0
givenIrisManhattanBasica_whenNeighbor_thenIrisNeighbor =
    TestCase (assertEqual "La suma manhattan és 25, l'arrel 5, el nom és 'Patata'"
              expected23
             (neighbor input23a input23b input23c input23d))

input24a = euclediana
input24b = ponderada
input24c = Iris ""    (Sepal 1.0 2.0) (Petal 3.0 0.0)
input24d = Iris "Patata" (Sepal 2.0 0.0) (Petal 1.0 0.0)
expected24 = IrisNeighbor "Patata" (1.0 / 3.0)
givenIrisEucledianaPonderada_whenNeighbor_thenIrisNeighbor =
    TestCase (assertEqual "La suma euclediana és 9, l'arrel 3, la inversa és 0.33, el nom és 'Patata'"
              expected24
             (neighbor input24a input24b input24c input24d))

input25a = manhattan
input25b = ponderada
input25c = Iris ""    (Sepal 12.0 2.0) (Petal 20.0 0.0)
input25d = Iris "Patata" (Sepal 5.0 10.0) (Petal 10.0 0.0)
expected25 = IrisNeighbor "Patata" 0.2
givenIrisManhattanPonderada_whenNeighbor_thenIrisNeighbor =
    TestCase (assertEqual "La suma manhattan és 25, l'arrel 5, la inversa és 0.2, el nom és 'Patata'"
              expected25
             (neighbor input25a input25b input25c input25d))

-- kNearestNeighbors :: Int -> Distancia -> Votacio -> [Iris] -> Iris -> [String]

input26a = 1
input26b = manhattan
input26c = 'P'
input26d = [ (Iris "Patata"   (Sepal 5.0 10.0) (Petal 10.0 0.0)),
             (Iris "Tomaquet" (Sepal 12.0 1.0) (Petal 20.0 0.0)),
             (Iris "Cotxe"    (Sepal 12.0 4.0) (Petal 20.0 0.0)) ]
input26e = Iris ""    (Sepal 12.0 2.0) (Petal 20.0 0.0)
expected26 = ["Tomaquet"]
givenK1Ponderada_whenKNeareastNeighbors_thenTomaquet =
    TestCase (assertEqual "L'ordre és Tomaquet, Cotxe i al final Patata"
              expected26
             (kNearestNeighbors input26a input26b input26c input26d input26e))

input27a = 1
input27b = manhattan
input27c = 'B'
input27d = [ (Iris "Patata"   (Sepal 5.0 10.0) (Petal 10.0 0.0)),
             (Iris "Tomaquet" (Sepal 12.0 1.0) (Petal 20.0 0.0)),
             (Iris "Cotxe"    (Sepal 12.0 4.0) (Petal 20.0 0.0)) ]
input27e = Iris ""    (Sepal 12.0 2.0) (Petal 20.0 0.0)
expected27 = ["Tomaquet"]
givenK1Basica_whenKNeareastNeighbors_thenTomaquet =
    TestCase (assertEqual "L'ordre és Tomaquet, Cotxe i al final Patata"
              expected27
             (kNearestNeighbors input27a input27b input27c input27d input27e))

input28a = 2
input28b = manhattan
input28c = 'B'
input28d = [ (Iris "Patata"   (Sepal 5.0 10.0) (Petal 10.0 0.0)),
             (Iris "Tomaquet" (Sepal 12.0 1.0) (Petal 20.0 0.0)),
             (Iris "Cotxe"    (Sepal 12.0 4.0) (Petal 20.0 0.0)) ]
input28e = Iris ""    (Sepal 12.0 2.0) (Petal 20.0 0.0)
expected28 = ["Tomaquet", "Cotxe"]
givenK2Basica_whenKNeareastNeighbors_thenTomaquet =
    TestCase (assertEqual "L'ordre és Tomaquet, Cotxe i al final Patata"
              expected28
             (kNearestNeighbors input28a input28b input28c input28d input28e))

-- knn :: Int -> [Iris] -> [Iris] -> Distancia -> Char -> [[String]]

input29a = 1
input29b = [ (Iris ""         (Sepal 12.0 1.0) (Petal 20.0 0.0)),
             (Iris ""         (Sepal 12.0 9.0) (Petal 20.0 0.0)),
             (Iris ""         (Sepal 12.0 3.0) (Petal 20.0 0.0)) ]
input29c = [ (Iris "Patata"   (Sepal 12.0 0.0) (Petal 20.0 0.0)),
             (Iris "Tomaquet" (Sepal 12.0 4.0) (Petal 20.0 0.0)),
             (Iris "Cotxe"    (Sepal 12.0 9.0) (Petal 20.0 0.0)) ]
input29d = manhattan
input29e = 'B'
expected29 = [ ["Patata"], ["Cotxe"], ["Tomaquet"] ]
givenK1NN_whenKNN_thenPatataCotxeTomaquet =
    TestCase (assertEqual "Troba la llista dels mes propers segons KNN"
              expected29
             (knn input29a input29b input29c input29d input29e))

input30a = 2
input30b = [ (Iris ""         (Sepal 12.0 1.0) (Petal 20.0 0.0)),
             (Iris ""         (Sepal 12.0 9.0) (Petal 20.0 0.0)),
             (Iris ""         (Sepal 12.0 3.0) (Petal 20.0 0.0)) ]
input30c = [ (Iris "Patata"   (Sepal 12.0 0.0) (Petal 20.0 0.0)),
             (Iris "Tomaquet" (Sepal 12.0 4.0) (Petal 20.0 0.0)),
             (Iris "Cotxe"    (Sepal 12.0 9.0) (Petal 20.0 0.0)) ]
input30d = manhattan
input30e = 'B'
expected30 = [ ["Patata", "Tomaquet"], ["Cotxe", "Tomaquet"], ["Tomaquet", "Patata"] ]
givenK2NN_whenKNN_thenPatatsTomaquetCotxeTomaquetTomaquetPatata =
    TestCase (assertEqual "Troba la llista dels mes propers segons KNN"
              expected30
             (knn input30a input30b input30c input30d input30e))

input31a = 4
input31b = [ (Iris ""         (Sepal 12.0 1.0) (Petal 20.0 0.0)),
             (Iris ""         (Sepal 12.0 9.0) (Petal 20.0 0.0)),
             (Iris ""         (Sepal 12.0 3.0) (Petal 20.0 0.0)) ]
input31c = [ (Iris "Patata"   (Sepal 12.0 1.1) (Petal 20.0 0.0)),
             (Iris "Patata"   (Sepal 12.0 0.9) (Petal 20.0 0.0)),
             (Iris "Patata"   (Sepal 12.0 3.1) (Petal 20.0 0.0)),
             (Iris "Tomaquet" (Sepal 12.0 3.2) (Petal 20.0 0.0)),
             (Iris "Tomaquet" (Sepal 12.0 3.3) (Petal 20.0 0.0)),
             (Iris "Tomaquet" (Sepal 12.0 3.4) (Petal 20.0 0.0)),
             (Iris "Cotxe"    (Sepal 12.0 9.1) (Petal 20.0 0.0)),
             (Iris "Cotxe"    (Sepal 12.0 9.1) (Petal 20.0 0.0)),
             (Iris "Cotxe"    (Sepal 12.0 9.1) (Petal 20.0 0.0)) ]
input31d = manhattan
input31e = 'B'
expected31 = [ ["Patata", "Patata", "Patata", "Tomaquet"],
               ["Cotxe", "Cotxe", "Cotxe", "Tomaquet"],
               ["Patata", "Tomaquet", "Tomaquet", "Tomaquet"] ]
givenK4NN_whenKNN_thenReturnWithRepetitions =
    TestCase (assertEqual "Troba la llista dels mes propers segons KNN"
              expected31
             (knn input31a input31b input31c input31d input31e))

-- irisKNN :: Int -> [Iris] -> [Iris] -> Distancia -> Char -> [String]

input32a = 3
input32b = [ (Iris ""         (Sepal 12.0 1.0) (Petal 20.0 0.0)),
             (Iris ""         (Sepal 12.0 9.0) (Petal 20.0 0.0)),
             (Iris ""         (Sepal 12.0 3.0) (Petal 20.0 0.0)) ]
input32c = [ (Iris "Patata"   (Sepal 12.0 1.1) (Petal 20.0 0.0)),
             (Iris "Patata"   (Sepal 12.0 0.9) (Petal 20.0 0.0)),
             (Iris "Patata"   (Sepal 12.0 3.1) (Petal 20.0 0.0)),
             (Iris "Tomaquet" (Sepal 12.0 3.2) (Petal 20.0 0.0)),
             (Iris "Tomaquet" (Sepal 12.0 3.3) (Petal 20.0 0.0)),
             (Iris "Tomaquet" (Sepal 12.0 3.4) (Petal 20.0 0.0)),
             (Iris "Cotxe"    (Sepal 12.0 9.1) (Petal 20.0 0.0)),
             (Iris "Cotxe"    (Sepal 12.0 9.1) (Petal 20.0 0.0)),
             (Iris "Cotxe"    (Sepal 12.0 9.1) (Petal 20.0 0.0)) ]
input32d = manhattan
input32e = 'B'
expected32 = [ "Patata", "Cotxe", "Tomaquet" ]
givenK3NN_whenIrisKNN_thenReturnMoreFrequent =
    TestCase (assertEqual "Troba el mes propers segons KNN"
              expected32
             (irisKNN input32a input32b input32c input32d input32e))

input33a = 1
input33b = [ (Iris ""         (Sepal 12.0 1.0) (Petal 20.0 0.0)),
             (Iris ""         (Sepal 12.0 9.0) (Petal 20.0 0.0)),
             (Iris ""         (Sepal 12.0 3.0) (Petal 20.0 0.0)) ]
input33c = [ (Iris "Patata"   (Sepal 12.0 1.1) (Petal 20.0 0.0)),
             (Iris "Patata"   (Sepal 12.0 0.9) (Petal 20.0 0.0)),
             (Iris "Patata"   (Sepal 12.0 3.1) (Petal 20.0 0.0)),
             (Iris "Tomaquet" (Sepal 12.0 3.2) (Petal 20.0 0.0)),
             (Iris "Tomaquet" (Sepal 12.0 3.3) (Petal 20.0 0.0)),
             (Iris "Tomaquet" (Sepal 12.0 3.4) (Petal 20.0 0.0)),
             (Iris "Cotxe"    (Sepal 12.0 9.1) (Petal 20.0 0.0)),
             (Iris "Cotxe"    (Sepal 12.0 9.1) (Petal 20.0 0.0)),
             (Iris "Cotxe"    (Sepal 12.0 9.1) (Petal 20.0 0.0)) ]
input33d = manhattan
input33e = 'B'
expected33 = [ "Patata", "Cotxe", "Patata" ]
givenK1NN_whenIrisKNN_thenReturnMoreFrequent =
    TestCase (assertEqual "Troba el mes propers segons KNN"
              expected33
             (irisKNN input33a input33b input33c input33d input33e))

-- applyAv :: [Iris] -> [String] -> (Float, Float)

input34a = [ (Iris "Patata"   (Sepal 12.0 1.1) (Petal 20.0 0.0)),
             (Iris "Patata"   (Sepal 12.0 0.9) (Petal 20.0 0.0)),
             (Iris "Patata"   (Sepal 12.0 3.1) (Petal 20.0 0.0)),
             (Iris "Tomaquet" (Sepal 12.0 3.2) (Petal 20.0 0.0)),
             (Iris "Tomaquet" (Sepal 12.0 3.3) (Petal 20.0 0.0)),
             (Iris "Tomaquet" (Sepal 12.0 3.4) (Petal 20.0 0.0)),
             (Iris "Cotxe"    (Sepal 12.0 9.1) (Petal 20.0 0.0)),
             (Iris "Cotxe"    (Sepal 12.0 9.1) (Petal 20.0 0.0)),
             (Iris "Cotxe"    (Sepal 12.0 9.1) (Petal 20.0 0.0)),
             (Iris "Cotxe"    (Sepal 12.0 9.1) (Petal 20.0 0.0)) ]
input34b = [ "Patata",
             "Cotxe",
             "Patata",
             "Hola",
             "Patata",
             "Tomaquet",
             "Cotxe",
             "Patata",
             "No",
             "Si" ]
expected34 = (0.4, 0.6)
givenResults_whenApplyAv_thenPair =
    TestCase (assertEqual "Pair of accuracies"
              expected34
             (applyAv input34a input34b))





