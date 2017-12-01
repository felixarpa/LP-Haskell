import Reader
import Iris
import KNN

welcome = "Benvingut al Iris k Nearest Neighbors"

kMessage = "Variable k (entre 1 i ∞):"
dMessage = "Funció de distancia. Euclediana (E) o Manhattan (M):"
vMessage = "Mecanisme de votació: Simple (S) o Ponderat (P):"

aMessage = "Avaluació accuracy o exactitud:"
lMessage = "Avaluació lost o error:"

main = do
    putStrLn welcome
    train <- readFile "iris.train.txt"
    test  <- readFile "iris.test.txt"
    putStrLn kMessage
    kLine <- getLine
    putStrLn dMessage
    dLine <- getLine
    putStrLn vMessage
    vLine <- getLine
    let aval = applyAv (parseI test) 
                       (irisKNN (parseK kLine)
                                (parseI test )
                                (parseI train)
                                (parseD dLine)
                                (parseV vLine))
    putStrLn aMessage
    putStrLn $ show $ fst aval
    putStrLn lMessage
    putStrLn $ show $ snd aval

