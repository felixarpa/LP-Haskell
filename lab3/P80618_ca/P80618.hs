data Queue a = Queue [a] [a]
    deriving (Show)

c = push 3 (push 2 (push 1 create))
c1 = push 4 (pop (push 3 (push 2 (push 1 create))))
let c2 = push 4 (push 3 (push 2 create))

create :: Queue a
create = Queue [] []

push :: a -> Queue a -> Queue a
push x (Queue as bs) = Queue as (x:bs)

pop :: Queue a -> Queue a
pop (Queue [] bs) = Queue (reverse $ init bs) []
pop (Queue (a:as) bs) = Queue as bs

top :: Queue a -> a
top (Queue [] bs) = top $ Queue (reverse bs) []
top (Queue (a:as) _) = a

empty :: Queue a -> Bool
empty (Queue [] []) = True
empty _ = False

instance Eq a => Eq (Queue a)
    where (Queue xa xb) == (Queue ya yb) = (xa ++ reverse (xb)) == (ya ++ (reverse yb))
