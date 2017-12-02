data Expr = Val Int | Add Expr Expr | Sub Expr Expr | Mul Expr Expr | Div Expr Expr


eval1 :: Expr -> Int
eval1 (Val x) = x
eval1 (Add x y) = (eval1 x) + (eval1 y)
eval1 (Sub x y) = (eval1 x) - (eval1 y)
eval1 (Mul x y) = (eval1 x) * (eval1 y)
eval1 (Div x y) = (eval1 x) `div` (eval1 y)


eval2 :: Expr -> Maybe Int
eval2 (Val x) = return x
eval2 (Add x y) = eval2' (+) x y
eval2 (Sub x y) = eval2' (-) x y
eval2 (Mul x y) = eval2' (*) x y
eval2 (Div x y) = eval2div   x y

safe2Div :: Int -> Int -> Maybe Int
safe2Div n m
    | m == 0    = Nothing
    | otherwise = Just (n `div` m)

eval2div :: Expr -> Expr -> Maybe Int
eval2div x y = do i <- eval2 x
                  j <- eval2 y
                  safe2Div i j

eval2' :: (Int -> Int -> Int) -> Expr -> Expr -> Maybe Int
eval2' f x y = do i <- eval2 x
                  j <- eval2 y
                  Just (f i j)


eval3 :: Expr -> Either String Int
eval3 (Val x) = Right x
eval3 (Add x y) = eval3' (+) x y
eval3 (Sub x y) = eval3' (-) x y
eval3 (Mul x y) = eval3' (*) x y
eval3 (Div x y) = eval3div   x y

safe3Div :: Int -> Int -> Either String Int
safe3Div n m
    | m == 0    = Left "div0"
    | otherwise = Right (n `div` m)

eval3div :: Expr -> Expr -> Either String Int
eval3div x y = do i <- eval3 x
                  j <- eval3 y
                  safe3Div i j

eval3' :: (Int -> Int -> Int) -> Expr -> Expr -> Either String Int
eval3' f x y = do i <- eval3 x
                  j <- eval3 y
                  Right (f i j)
