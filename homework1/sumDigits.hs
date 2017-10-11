--sumDigits.hs--
--[Integer] -> Integer --

sumDigits [] = 0
sumDigits [x] = x
sumDigits [s:ns] = sumDigits (toDigits(s)) + sumDigits ns