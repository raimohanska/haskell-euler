-- If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, how many letters would be used?

import Data.List

inWords :: Int -> String
inWords n | n < 20 = (!!) words n
    where words = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]
inWords n | n < 100 = (!!) words ((n `div` 10) - 2) ++ " " ++ inWords (n `mod` 10)
    where words = ["twenty", "thirty", "fourty", "fifty", "sixty", "seventy", "eighty", "ninety"]