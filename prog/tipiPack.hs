--tipiPack.hs, data tipi, progetto parser prima parte--

--Pack {tag,arity}--
data Colour= Red | Green | Blue
Red = Pack {1,0}
Green = Pack {2,0}
Blue = Pack {3,0}
data Complex= Rect Num Num | Polar Num Num
Rect = Pack {1,2}
Polar = Pack {2,2}
data Tree a = Leaf a | Node (Tree a) (Tree a)
Tree = Pack {1,1}
Node = Pack {2,2}

--type checking guarantees that different type are not mixed--
