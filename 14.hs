-- Which starting number, under one million, produces the longest chain?

import List       
import qualified Data.IntMap as M

longest n = snd $ maximum $ chainLengths [1..n]

chainLengths [] = []
chainLengths (n:ns) = (chainLength n, n): chainLengths ns

chainLength n
    |n==1 = 1
    |even n = 1 + chainLength (n `div` 2)
    |otherwise = 1 + chainLength (3*n + 1)
    
-- todo: try if IntMap can speed this up
loop :: Int -> Int    
loop n = mapWith n M.empty
    where mapWith 1 _ = 0
          mapWith n mapped = mapWith (n-1) (M.insert n n mapped)