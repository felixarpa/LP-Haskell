data Tree a = Node a (Tree a) (Tree a) | Empty deriving (Show)

t7 = Node 7 Empty Empty
t6 = Node 6 Empty Empty
t5 = Node 5 Empty Empty
t4 = Node 4 Empty Empty
t3 = Node 3 t6 t7
t2 = Node 2 t4 t5
t1 = Node 1 t2 t3
t1' = Node 1 t3 t2

size :: Tree a -> Int
size Empty = 0
size (Node _ a b) = 1 + size a + size b

height :: Tree a -> Int
height Empty = 0
height (Node _ a b) = 1 + max (height a) (height b)

equal :: Eq a => Tree a -> Tree a -> Bool
equal Empty (Node _ _ _) = False
equal (Node _ _ _) Empty = False
equal Empty Empty = True
equal (Node x1 a1 b1) (Node x2 a2 b2) = x1 == x2 && 
                                        equal a1 a2 &&
                                        equal b1 b2

isomorphic :: Eq a => Tree a -> Tree a -> Bool
isomorphic Empty (Node _ _ _) = False
isomorphic (Node _ _ _) Empty = False
isomorphic Empty Empty = True
isomorphic (Node x1 a1 b1) (Node x2 a2 b2) = x1 == x2 &&
                                           ((equal a1 a2 &&
                                             equal b1 b2) ||
                                            (equal a1 b2 &&
                                             equal b1 a2))

preOrder :: Tree a -> [a]
preOrder Empty = []
preOrder (Node x a b) = [x] ++ (preOrder a) ++ (preOrder b)

postOrder :: Tree a -> [a]
postOrder Empty = []
postOrder (Node x a b) = (postOrder a) ++ (postOrder b) ++ [x]

inOrder :: Tree a -> [a]
inOrder Empty = []
inOrder (Node x a b) = (inOrder a) ++ [x] ++ (inOrder b)

breadthFirst :: Tree a -> [a]
breadthFirst x = breadthFirstRec [x]

breadthFirstRec :: [Tree a] -> [a]
breadthFirstRec [] = []
breadthFirstRec (Empty:ts) = breadthFirstRec ts
breadthFirstRec ((Node x left right):ts) = x : (breadthFirstRec $ ts ++ [left, right])

build :: Eq a => [a] -> [a] -> Tree a
build [ ] [ ]              = Empty
build (x:preorder) inorder = Node x
                         (build leftPreorder  leftInorder )
                         (build rightPreorder rightInorder)
--    where  
        leftInorder   = takeWhile (/= x) inorder
        leftPreorder  = take (length leftInorder) preorder
        rightPreorder = drop (length leftInorder) preorder
        rightInorder  = tail (dropWhile (/= x) inorder)

overlap :: (a -> a -> a) -> Tree a -> Tree a -> Tree a
overlap _ tree1 Empty = tree1
overlap _ Empty tree2 = tree2
overlap op (Node x1 a1 b1) (Node x2 a2 b2) = Node (foldl op x1 [x2])
                                             (overlap op a1 a2)
                                             (overlap op b1 b2)

