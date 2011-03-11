-- What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral formed in the same way?

-- Numbers per shell 1=>8, 2=>16
count n = n*8
-- Starting number 1=>2, 2=>10, 3=>26
st 0 = 1
st 1 = 2
st n = st (n-1) + count (n-1)
-- Diagonals: 1=>3,5,7,9    2=>13,17,21,25
diags 0 = [1]
diags n = map diag [1 .. 4]
  where diag i = base + i * fourth
        base = st n - 1
        fourth = (count n) `div` 4
-- # shells for n*n spiral: 1=>1, 3=>2, 5=>3
shells n = (n + 1) `div` 2

diagSum n = sum $ concat $ map diags [0 .. (shells n) - 1]