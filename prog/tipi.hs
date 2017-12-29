--tipi.hs, data tipi, progetto parser prima parte--

data Colour= Red | Green | Blue
data Complex= Rect Int Int | Polar Int Int
data Tree a = Leaf a | Node (Tree a) (Tree a)
data NumPair = MkNumPair Int Int
