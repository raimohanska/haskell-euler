-- A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 * 99.
-- Find the largest palindrome made from the product of two 3-digit numbers.                                                            

import Data.List     
import GHC.Exts(sortWith)

largestPalindrome = head $ sortWith (0 -) $ (filter palindrome) $ products [100..999]  [100..999]

palindrome x = (digitsOf x) == (reverse $ digitsOf x)

digitsOf x | x < 10    = [x]
           | otherwise = digitsOf (x `div` 10) ++  digitsOf (x `mod` 10)

products [] ys = []
products (x:xs) ys = map (* x) ys ++ (products xs ys)