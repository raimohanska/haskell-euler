-- 145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.
-- Find the sum of all numbers which are equal to the sum of the factorial of their digits.
-- Note: as 1! = 1 and 2! = 2 are not sums they are not included.

sumOfThose = sum $ filter equalToFactorialSum $ [10 .. 10^upperLimitForDigits]

upperLimitForDigits = head $ filter (impossible) $ [2..]
  where impossible n = 10 ^ (n-1) > n * (factorial 9)

equalToFactorialSum n = n == (sum $ map factorial $ digitsOf n)

factorial n = product [1..n]

digitsOf x | x < 10    = [x]
           | otherwise = digitsOf (x `div` 10) ++  digitsOf (x `mod` 10)
