--hanoi.hs--
hanoi 0 a b c = []
hanoi n a b c = hanoi (n-1) a c b ++ [(a,b)] ++ hanoi (n-1) c b a