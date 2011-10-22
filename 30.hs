-- Find the sum of all the numbers that can be written as the sum of fifth powers of their digits.

sumOfNumbers = sum $ filter equalsSumOfDigitsToFifth $ [2..maxVal]


equalsSumOfDigitsToFifth x = x == sumDigitsToFifth x

sumDigitsToFifth = sum . map (^5) . digitsOf

digitsOf x | x < 10    = [x]
           | otherwise = digitsOf (x `div` 10) ++  digitsOf (x `mod` 10)

maxVal = 10^maxDigits - 1
maxDigits = last $ takeWhile canHasDigits [1..]
canHasDigits n = allNines <= (sumDigitsToFifth allNines)
  where allNines = 10 ^ (n-1) - 1  
