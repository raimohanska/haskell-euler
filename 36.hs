-- The decimal number, 585 = 10010010012 (binary), is palindromic in both bases.
-- Find the sum of all numbers, less than one million, which are palindromic in base 10 and base 2.

import Data.List     
import GHC.Exts(sortWith)

dpalSum = sum dpals
dpals = filter dpal $ [1..999999]
dpal x = (palindrome 10 x) && (palindrome 2 x)

palindrome base x = (digitsOf base x) == (reverse $ digitsOf base x)

digitsOf base x | x < base    = [x]
                | otherwise = digitsOf base (x `div` base) ++ digitsOf base (x `mod` base)
