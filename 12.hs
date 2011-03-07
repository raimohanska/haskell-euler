-- What is the value of the first triangle number to have over five hundred divisors?

triangles = scanl1 (+) [1 ..]

factors n = filter (`factorOf` n) [1 .. n]
    where factorOf a b = b `mod` a == 0
    
firstOneWithMoreThan divisorCount = head $ dropWhile notEnoughFactors triangles      
    where notEnoughFactors n = (length $ factors n) <= divisorCount