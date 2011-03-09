-- Which starting number, under one million, produces the longest chain?

next :: Int -> Int
next n | n `mod` 2 == 0 = n `div` 2
       | otherwise      = 3 * n + 1
       
chain 1 = [1]
chain n = n : (chain $ next n)

longestChain limit = fst $ foldr longer (0, 0) $ map withLength [1 .. limit]
    where longer (n, lenN) (m, lenM) | lenN > lenM = (n, lenN)
                                     | otherwise   = (m, lenM)
          withLength n = (n, length $ chain n)