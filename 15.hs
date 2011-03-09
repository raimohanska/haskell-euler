-- How many routes are there through a 20x20 grid?

routes :: Int -> Int -> Int
routes 0 n = 1
routes n 0 = 1
routes n m = (routes (n-1) m) + (routes n (m-1))

-- For dimensions D*D                            
-- 2D moves total -> (2D)! ways to order the list of moves
-- the D down moves and the D right moves can each be ordered in D! ways
-- this gives the total of (2D)! / (D!)^2 routes

fastRoutes n = (factorial (2 * n)) `div` ((factorial n) ^ 2)
factorial n = product [1..n]