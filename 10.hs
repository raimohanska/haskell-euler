-- The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
-- Find the sum of all the primes below two million.

sumBelow2Million = sum $ takeWhile (< 2000000) primes
primes = 2 : 3 : filter prime [5 ..]
prime x = factors x == []
factors x = filter (`factorOf` x) $ takeWhile (\y -> y ^ 2 <= x) primes          
factorOf x y = y `mod` x == 0
