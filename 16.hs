-- What is the sum of the digits of the number 2^1000?

digitsOf x | x < 10    = [x]
           | otherwise = digitsOf (x `div` 10) ++  digitsOf (x `mod` 10)

digitSum n = sum $ digitsOf n