-- Find the sum of all the positive integers which cannot be written as the sum of two abundant numbers.
-- All integers greater than 28123 can be written as the sum of two abundant numbers. 

abundant x = x < sumProperDivisors x

sumProperDivisors = sum . properDivisors
    where properDivisors x = filter (`divisorOf` x) [1..x `div`2]          
          divisorOf x y = y `mod` x == 0
          
abundantNumbers = filter abundant [1..28122]         