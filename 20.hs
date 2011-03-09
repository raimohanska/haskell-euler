-- Find the sum of the digits in the number 100!

factorial n = product [1..n]
digitsOf x | x < 10    = [x]
           | otherwise = digitsOf (x `div` 10) ++  digitsOf (x `mod` 10)

digitSum n = sum $ digitsOf n   
