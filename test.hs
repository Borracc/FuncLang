-- test.hs --
doubleMe x = x + x
quadrupleMe x = doubleMe (doubleMe x)
doubleUs x y = x*2 + y*2