--A Pythagorean triplet is a set of three natural numbers, a  b  c, for which,

--a2 + b2 = c2
--For example, 32 + 42 = 9 + 16 = 25 = 52.

--There exists exactly one Pythagorean triplet for which a + b + c = 1000.
--Find the product abc.

abPairs = pairsFor [1..500]
    where 
        pairsFor []     = []
        pairsFor (a:as) = map (pair a) [1 .. (a-1)] ++ pairsFor as
        pair a b = (a, b)
        
triplets = map triplet abPairs
    where 
        triplet (a, b) = (a, b, 1000 - a -b)
                                     
pythagorian (a, b, c) = (a^2 + b^2 == c^2)

tripletProduct (a, b, c) = a * b * c

pythagorianTripletProduct = tripletProduct $ head $ filter pythagorian triplets