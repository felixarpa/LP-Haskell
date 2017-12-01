import Data.List -- 
import Data.Ord  

votsMinim :: [([Char], Int)] -> Int -> Bool
votsMinim candidats votsMin = or $ map (menys votsMin) candidats
    where
        menys minim (_, vots) = vots < minim -- snd candidat < minim

-- utilitzem comparing de Data.Ord i maximumBy de Data.List
candidatMesVotat :: [([Char], Int)] -> [Char]
candidatMesVotat cs = fst $ maximumBy (comparing snd) cs

votsIngressos :: [([Char], Int)] -> [([Char], Int)] -> [[Char]]
votsIngressos cs []     = map (fst) cs
votsIngressos cs (i:is) = votsIngressos (votsIngressos' cs i) is
    where
        votsIngressos' :: [([Char], Int)] -> ([Char], Int) -> [([Char], Int)]
        votsIngressos' [] _ = []
        votsIngressos' (x:xs) y
            | (fst x) == (fst y) = votsIngressos' xs y
            | otherwise          = x:(votsIngressos' xs y)

-- utilitzem comparing de Data.Ord i sortBy de Data.List
rics :: [([Char], Int)] -> [([Char], Int)] -> [[Char]]
rics cs is = map (candidat cs) $ map (fst) $ take 3 $ reverse $ sortBy (comparing snd) is
    where
        candidat :: [([Char], Int)] -> [Char] -> [Char]
        candidat cc ric
            | elem ric (map (fst) cc) = ric ++ "*"
            | otherwise               = ric