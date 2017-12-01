import Reader
import Iris
import KNN
import Files

main = do

    putStrLn welcome

    train <- readFile trainFile
    test  <- readFile testFile
    
    let maxim = length $ parseI train
    kLine <- get (kMessage maxim) (map (show) [1..maxim])
    dLine <- get dMessage dValues
    vLine <- get vMessage vValues 
    
    let aval = applyAv (parseI test) 
                       (irisKNN (parseK kLine)
                                (parseI test )
                                (parseI train)
                                (parseD dLine)
                                (parseV vLine))
    
    putStr aMessage
    putStr $ show $ fst aval
    putStr lMessage
    putStr $ show $ snd aval

    putStrLn bye

