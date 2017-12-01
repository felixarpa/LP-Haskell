import Test.HUnit
import Reader
import Iris

-- https://hackage.haskell.org/package/HUnit
-- runTestTT tests

readerTests = TestList [ given3LinesText_whenTextToLines_thenReturnArrayOfLines,
                         givenTextWithCommas_whenLineToArray_thenReturnArrayOfWords,
                         givenArrayOfValues_whenArrayToIris_thenReturnIris,
                         givenText_whenParseIris_thenReturnIrisArray ]

-- TEST LIST


-- textToLines :: String -> [String]

input01a = "Fèlix\nArribas\nPardo"
expected01 = [ "Fèlix", "Arribas", "Pardo" ]
given3LinesText_whenTextToLines_thenReturnArrayOfLines = 
    TestCase (assertEqual "Separa en linies"
              expected01
             (textToLines input01a ))


-- lineToArray :: String -> [String]

input02a = "6.1,2.9,4.7,1.4,Iris-versicolor"
expected02 = [ "6.1", "2.9", "4.7", "1.4", "Iris-versicolor" ]
givenTextWithCommas_whenLineToArray_thenReturnArrayOfWords = 
    TestCase (assertEqual "Separa per comes"
              expected02
             (lineToArray input02a ))


-- arrayToIris :: String -> [String]

input03a = [ "6.1", "2.9", "4.7", "1.4", "Iris-versicolor" ]
expected03 = Iris "Iris-versicolor" (Sepal 6.1 2.9) (Petal 4.7 1.4)
givenArrayOfValues_whenArrayToIris_thenReturnIris = 
    TestCase (assertEqual "Crea un Iris donat un array"
              expected03
             (arrayToIris input03a ))


-- parseI :: String -> [Iris]

input04a = "6.1,0.9,4.7,1.4,Iris\n6.1,2.9,4.7,1.4,Aras\n6.1,2.9,4.7,9.4,Eres"
expected04 = [ (Iris "Iris" (Sepal 6.1 0.9) (Petal 4.7 1.4)),
               (Iris "Aras" (Sepal 6.1 2.9) (Petal 4.7 1.4)),
               (Iris "Eres" (Sepal 6.1 2.9) (Petal 4.7 9.4)) ]
givenText_whenParseIris_thenReturnIrisArray = 
    TestCase (assertEqual "Crea un array d'Iris donat un text"
              expected04
             (parseI input04a ))