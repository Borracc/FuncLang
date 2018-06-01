-- test.hs --
doubleMe x = x + x
quadrupleMe x = doubleMe (doubleMe x)
doubleUs x y = x*2 + y*2

-- altre funzioni di prova --
signumMio n | n<0 = (-1)
            | n==0 = 0
            | otherwise = 1

inc :: [Int] -> [Int]
inc [] = []
inc (n:ns) = n+1: inc ns

sqr :: [Int] -> [Int]
sqr [] = []
sqr (n:ns) = n^2: sqr ns

--test Data--
data Shape = Circle Float | Rect Float Float
    deriving(Show)

area :: Shape -> Float
area (Circle r) = pi * r^2
area (Rect x y) = x * y

data Tree a = Leaf a | Node (Tree a) (Tree a)
    deriving(Show)

instance Functor Tree where
--fmap :: (a -> b) -> f(a) -> f(b)
--fmap :: (a -> b) -> Tree a -> Tree b
	fmap g (Leaf x) = Leaf (g(x))
	fmap g (Node l r) = Node (fmap g l) (fmap g r)

--Data Maybe--
