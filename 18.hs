-- Find the maximum total from top to bottom of the triangle in 18.txt
-- Solves problem 67 as well

import GHC.Exts                                                                    

main = do
    solve "18"
    solve "67"
 where solve n = do
        str <- readFile $ n ++ ".txt"
        putStrLn $ "Solution for #" ++ n ++ ": " ++ (show $ maximum $ maxes $ toLists $ str)
    
toLists = reverse . map (\line -> map readInt $ words line) . lines
    where readInt str = read str :: Int
                   
maxToNext list = zipWith max (0 : list) (list ++ [0])
                                                             
maxes [] = []
maxes (list : lists) = zipWith (+) list (maxToNext (maxes lists))