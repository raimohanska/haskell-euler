-- The prime factors of 13195 are 5, 7, 13 and 29.
-- What is the largest prime factor of the number 600851475143 ?

largestPrimeFactor = last $ primeFactors 600851475143
primeFactors = filter prime . factors
    where prime x = factors x == []
          factors x = filter (`factorOf` x) $ takeWhile (\y -> y ^ 2 <= x) [2 .. ]
          
factorOf x y = y `mod` x == 0
