data Tree a = Leaf a | Node a (Tree a) (Tree a)
--i
--k (Leaf x) g = Leaf x
--k (Node x y z) g = Node (g x) (k y g) (k z g)
--k:: Tree t -> (t -> t) -> Tree t

--ii
k (Leaf x) g = Leaf (g x)
k (Node x y z) g = Node (g x) (k y g) (k z g)
--k:: Tree t -> (t -> t) -> Tree t

--iii
--k (Leaf x) g = (g x)
--k (Node x y z) g = Node (g x) (k y g) (k z g)
--k:: ** TYPE ERROR **

--iv
--k (Leaf x) g = x
--k (Node x y z) g = Node (g x) (k y g) (k z g)
--k:: Tree (Tree a) -> (Tree a -> a) -> Tree a