-- Evaluate the sum of all the amicable numbers under 10000.

import Control.Parallel (par)

sumAmicable n = sum $ filter amicable [1..n]                         
amicable x = (x /= pair) && (x == (sumProperDivisors $ pair))
    where pair = sumProperDivisors x
sumProperDivisors = sum . properDivisors
    where properDivisors x = filter (`divisorOf` x) [1..x `div`2]          
          divisorOf x y = y `mod` x == 0                   