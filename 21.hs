-- Evaluate the sum of all the amicable numbers under 10000.

sumAmicable n = sum $ filter amicable [1..n]                         
amicable x = x == (sumProperDivisors $ sumProperDivisors x)
sumProperDivisors = sum . properDivisors
    where properDivisors x = filter (`divisorOf` x) [1..x `div`2]          
          divisorOf x y = y `mod` x == 0                   
          
-- TODO: trying to optimize using prime factors etc          
          
primeFactors x = pf x primes
     where pf x (p:ps) | p > x = []
                       | p `factorOf` x = p : (pf (x `div` p) (p:ps))
                       | otherwise = pf x ps
     
primes = 2 : 3 : filter prime [5 ..]
prime x = factors x == []
factors x = filter (`factorOf` x) $ takeWhile (\y -> y ^ 2 <= x) primes          
factorOf x y = y `mod` x == 0      