--toDigitsRev.hs--
--Integer -> [Integer]--

toDigitsRev x | x<=0 = []
toDigitsRev x | x>0 = [mod x 10] ++ toDigitsRev (div x 10) 