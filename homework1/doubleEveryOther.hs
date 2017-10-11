--doubleEveryOther.hs--
--[Integer] -> [Integer]--

doubleEveryOther [] = []
doubleEveryOther [n] = [n]
doubleEveryOther (x:y:ns) | mod(length ns) 2 == 0 = [2*x,y] ++ doubleEveryOther ns
doubleEveryOther (x:y:ns) | otherwise = [x, y*2] ++ doubleEveryOther ns