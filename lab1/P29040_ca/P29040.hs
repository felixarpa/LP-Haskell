------------------------------------------------------------------------------------
-------------------------------------- INSERT --------------------------------------
------------------------------------------------------------------------------------
insert :: [Int] -> Int -> [Int]
insert [] x = [x]
insert (head:tail) x
    | head > x = x:head:tail
    | otherwise = head:(insert tail x)

------------------------------------------------------------------------------------
-------------------------------------- ISORT ---------------------------------------
------------------------------------------------------------------------------------
isort :: [Int] -> [Int]
isort [] = []
isort (head:tail) = insert (isort tail) head

------------------------------------------------------------------------------------
-------------------------------------- REMOVE --------------------------------------
------------------------------------------------------------------------------------
remove :: [Int] -> Int -> [Int]
remove [] _ = []
remove (x:l) y
    | x == y = l
    | otherwise = x:(remove l y)

------------------------------------------------------------------------------------
-------------------------------------- SSORT ---------------------------------------
------------------------------------------------------------------------------------
ssort :: [Int] -> [Int]
ssort [] = []
ssort (head:tail) = insert (ssort (remove (head:tail) head)) head

------------------------------------------------------------------------------------
-------------------------------------- MERGE ---------------------------------------
------------------------------------------------------------------------------------
merge :: [Int] -> [Int] -> [Int]
merge l1 [] = l1
merge [] l2 = l2
merge (head1:tail1) (head2:tail2)
    | head1 <= head2 = [head1] ++ (merge tail1 (head2:tail2))
    | head1 >  head2 = [head2] ++ (merge (head1:tail1) tail2)

------------------------------------------------------------------------------------
-------------------------------------- MSORT ---------------------------------------
------------------------------------------------------------------------------------
msort :: [Int] -> [Int]
msort [] = []
msort [last] = [last]
msort array = merge
                (msort $ take (half_length array) array)
                (msort $ drop (half_length array) array)
    where
        half_length = flip div 2 . length


------------------------------------------------------------------------------------
-------------------------------------- QSORT ---------------------------------------
------------------------------------------------------------------------------------
qsort :: [Int] -> [Int]
qsort xs = qsort' xs []
    where
        qsort' :: [Int] -> [Int] -> [Int]
        qsort' [] result = result
        qsort' [x] result = x:result
        qsort' (x:xs) result = qpart xs [] [] result
            where
                qpart :: [Int] -> [Int] -> [Int] -> [Int] -> [Int]
                qpart [] half1 half2 result = qsort' half1 (x:qsort' half2 result)
                qpart (x':xs') half1 half2 result
                    | x' <= x = qpart xs' (x':half1) half2 result
                    | x' >  x = qpart xs' half1 (x':half2) result

------------------------------------------------------------------------------------
------------------------------------- GENQSORT -------------------------------------
------------------------------------------------------------------------------------
genQsort :: Ord a => [a] -> [a]
genQsort xs = genQsort' xs []
    where
        genQsort' [] result = result
        genQsort' [x] result = x:result
        genQsort' (x:xs) result = genQpart xs [] [] result
            where
                genQpart [] half1 half2 result = genQsort' half1 (x:genQsort' half2 result)
                genQpart (x':xs') half1 half2 result
                    | x' <= x = genQpart xs' (x':half1) half2 result
                    | x' >  x = genQpart xs' half1 (x':half2) result

