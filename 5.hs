-- 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.

-- What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?

smallest = head $ dropWhile (not . divisible) [20, 40 ..]
divisible x = all (`factorOf` x) [19, 18, 17, 16, 15, 14, 13, 12, 11]
factorOf x y = y `mod` x == 0
          