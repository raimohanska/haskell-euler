-- 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.

-- What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?

smallest = foldr (*) 1 $ combinedFactors [1 .. 20]

factors 1 = []
factors n | firstFactor == n = [n]
          | otherwise   = factors firstFactor ++ factors (n `div` firstFactor)
          where firstFactor = head $ dropWhile (\x -> n `mod` x > 0) [2 ..]   
                                            
combineFactors :: [Int] -> [Int] -> [Int]          
combineFactors [] bs = bs
combineFactors as [] = as
combineFactors (a:as) (b:bs) | a == b = a : combineFactors as bs
                             | a < b  = a : combineFactors as (b:bs)
                             | a > b  = b : combineFactors (a:as) bs
                             
combinedFactors :: [Int] -> [Int]
combinedFactors as = foldr combineFactors [] (map factors as)
