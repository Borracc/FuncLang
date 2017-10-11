--sumDigits.hs--
--Integer -> [Integer]--
toDigits x | x<=0 = []
toDigits x | x>0 = toDigits (div x 10) ++ [mod x 10]

--[Integer] -> Integer --
sumDigits [] = 0
sumDigits [s] = s
sumDigits (s:ns) = sumDigits (toDigits (s) ) + sumDigits ns