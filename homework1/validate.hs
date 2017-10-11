--validate.hs
--Integer -> [Integer]
toDigits x | x<=0 = []
toDigits x | x>0 = toDigits (div x 10) ++ [mod x 10]

--[Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [n] = [n]
doubleEveryOther (x:y:ns) | mod(length ns) 2 == 0 = [2*x,y] ++ doubleEveryOther ns
doubleEveryOther (x:y:ns) | otherwise = [x, y*2] ++ doubleEveryOther ns

--[Integer] -> Integer --
sumDigits [] = 0
sumDigits [s] = s
sumDigits (s:ns) = sumDigits (toDigits (s) ) + sumDigits ns

--Integer -> Bool
validate x = mod (sumDigits( doubleEveryOther (toDigits (x)))) 10 == 0