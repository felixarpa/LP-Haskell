data AVL a = E | N a Int (AVL a) (AVL a) deriving (Show)

insert :: Ord a => AVL a -> a -> AVL a
insert E y = (N y 0 E E)


create :: Ord a => [a] -> AVL a
create [] = E
create (x:xs) = foldl insert (N x 0 E E) xs


--check :: AVL a -> (Bool,Int)