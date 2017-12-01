data Avi = Avi { nom :: [Char], edat :: Int, despeses :: [Int] } deriving (Show)

avis = [ Avi { nom = "Joan", edat = 68, despeses = [640, 589, 573]}, Avi { nom = "Pepa", edat = 69, despeses = [710,550,570,698,645,512]}, Avi { nom = "Anna", edat = 72, despeses = [530,534]}, Avi { nom = "Pep", edat = 75, despeses = [770,645,630,650,590,481,602]} ]

promigDespeses :: Avi -> Int
promigDespeses avi = round ((fromIntegral (sum    (despeses avi))) 
                          / (fromIntegral (length (despeses avi))))

edatsExtremes :: [Avi] -> (Int, Int)
edatsExtremes avis = (mesPetit avis, mesGran avis)
    where
        mesGran :: [Avi] -> Int
        mesGran  (a:as) = foldl max (edat a) (map (edat) as)
        mesPetit :: [Avi] -> Int
        mesPetit (b:bs) = foldl min (edat b) (map (edat) bs)

sumaPromig :: [Avi] -> Int
sumaPromig avis = sum $ map promigDespeses avis

maximPromig :: [Avi] -> Int
maximPromig (a:as) = foldl max (promigDespeses a) (map (promigDespeses) as)

despesaPromigSuperior :: [Avi] -> Int -> ([Char], Int)
despesaPromigSuperior [] _   = ("", 0)
despesaPromigSuperior (a:as) d
    | promigDespeses a >= d = (nom a, edat a)
    | otherwise             = despesaPromigSuperior as d