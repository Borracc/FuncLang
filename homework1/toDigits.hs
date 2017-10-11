--toDigits.hs--
--Integer -> [Integer]--

toDigits x | x<=0 = []
toDigits x | x>0 = toDigits (div x 10) ++ [mod x 10]