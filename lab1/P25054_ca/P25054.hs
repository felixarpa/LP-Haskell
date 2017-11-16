------------------------------------------------------------------------------------
------------------------------------- MYLENGTH -------------------------------------
------------------------------------------------------------------------------------
myLength :: [Int] -> Int
myLength [] = 0
myLength (_:tail) = 1 + (myLength tail)

------------------------------------------------------------------------------------
------------------------------------- MYMAXIMUM ------------------------------------
------------------------------------------------------------------------------------
myMaximum :: [Int] -> Int
myMaximum [last] = last
myMaximum (head:tail) = max head (myMaximum tail)

------------------------------------------------------------------------------------
------------------------------------- AVERAGE --------------------------------------
------------------------------------------------------------------------------------
totalSum :: [Int] -> Float
totalSum [] = 0.0
totalSum (head:tail) = fromIntegral head + (totalSum tail)

average :: [Int] -> Float
average array = totalSum array / fromIntegral (myLength array)

------------------------------------------------------------------------------------
------------------------------------ PALINDROME ------------------------------------
------------------------------------------------------------------------------------
reverseArray :: [Int] -> [Int]
reverseArray [last] = [last]
reverseArray (head:tail) = (reverseArray tail) ++ [head]

buildPalindrome :: [Int] -> [Int]
buildPalindrome [] = []
buildPalindrome array = (reverseArray array) ++ (array)

------------------------------------------------------------------------------------
-------------------------------------- REMOVE --------------------------------------
------------------------------------------------------------------------------------
remove :: [Int] -> [Int] -> [Int]
remove l1 [] = l1
remove l1 (x:l2) = remove (remove' l1 x) l2
    where 
        remove' :: [Int] -> Int -> [Int]
        remove' [] _ = []
        remove' (x:l) y
            | x == y = remove' l y
            | otherwise = x:(remove' l y)
------------------------------------------------------------------------------------
-------------------------------------- FLATTEN -------------------------------------
------------------------------------------------------------------------------------
flatten :: [[Int]] -> [Int]
flatten [] = []
flatten [lastArray] = lastArray
flatten (head:tail) = head ++ flatten tail

------------------------------------------------------------------------------------
------------------------------------ ODDSNEVENS ------------------------------------
------------------------------------------------------------------------------------
oddsNevens :: [Int] -> ([Int], [Int])
oddsNevens [] = ([],[])
oddsNevens array = (oddsNevens' array [] [])
    where
        oddsNevens' :: [Int] -> [Int] -> [Int] -> ([Int], [Int])
        oddsNevens' [] odds evens = (odds, evens)
        oddsNevens' (head:tail) odds evens
            | even head = (oddsNevens' tail odds (evens ++ [head]))
            | otherwise = (oddsNevens' tail (odds ++ [head]) evens)

------------------------------------------------------------------------------------
---------------------------------- PRIMERDIVISOR -----------------------------------
------------------------------------------------------------------------------------
primeDivisors :: Int -> [Int]
primeDivisors a = primeDivisorsRec 2
    where
        primeDivisorsRec :: Int -> [Int]
        primeDivisorsRec n
            | n == a + 1 = []
            | mod a n == 0 && isPrime n   = n : primeDivisorsRec (n+1)
            | otherwise = primeDivisorsRec (n+1)
            where
                isPrime :: Int -> Bool
                isPrime 0 = False
                isPrime 1 = False
                isPrime n = not (hasDivisors (n-1))
                    where
                        hasDivisors :: Int -> Bool
                        hasDivisors 1 = False
                        hasDivisors x = mod n x == 0 || hasDivisors (x-1)