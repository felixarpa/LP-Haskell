data BST a = E | N a (BST a) (BST a) deriving (Show)

insert :: Ord a => BST a -> a -> BST a
insert E y = (N y E E)
insert (N x fe fd) y
    | y == x = N x fe fd
    | y <  x = N x (insert fe y) fd
    | y >  x = N x fe (insert fd y)

create :: Ord a => [a] -> BST a
create [] = E
create (x:xs) = foldl insert (N x E E) xs

remove :: Ord a => BST a -> a -> BST a
remove E _ = E
remove (N x fe fd) y
    | y == x = create (elements fe ++ elements fd)
    | y <  x = N x (remove fe y) fd
    | y >  x = N x fe (remove fd y)

createWithout :: Ord a => [a] -> a -> BST a
createWithout xs y = create (takeWhile (<y) xs ++ dropWhile (<=y) xs)

contains :: Ord a => BST a -> a -> Bool
contains E _ = False
contains (N x fe fd) y
    | y == x = True
    | y <  x = contains fe y
    | y >  x = contains fd y

getmax :: BST a -> a
getmax (N x _ E) = x
getmax (N x _ fd) = getmax fd

getmin :: BST a -> a
getmin (N x E _) = x
getmin (N x fe _) = getmin fe

size :: BST a -> Int
size E = 0
size (N _ fe fd) = 1 + size fe + size fd

elements :: BST a -> [a]
elements E = []
elements (N x fe fd) = (elements fe) ++ [x] ++ (elements fd)