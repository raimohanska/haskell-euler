-- If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, how many letters would be used?
-- Do not count spaces or hyphens

-- TODO: still incorrect

import Data.List

countLetters = length $ filter (/= ' ') $ concat $ map inWords $ [1..1000]

inWords :: Int -> String
inWords n | n < 20 = (!!) words n
    where words = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]
-- TODO: outputs "thirty zero" etc    
inWords n | n < 100 = (!!) words ((n `div` 10) - 2) ++ " " ++ rest (n `mod` 10)
    where words = ["twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"]
          rest 0 = ""
          rest x = inWords x
inWords n | n < 1000 = inWords (n `div` 100) ++ " hundred" ++ rest (n `mod` 100)
    where rest 0 = ""
          rest x = " and " ++ inWords x
inWords 1000 = "one thousand"