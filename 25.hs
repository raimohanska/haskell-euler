-- What is the first term in the Fibonacci sequence to contain 1000 digits?

fib = 1 : 1 : zipWith (+) fib (tail fib)       
fibWithOrdinal = fib `zip` [1..]
           
firstThat :: (a -> Bool) -> [a] -> a
firstThat condition = head . dropWhile (\x -> not $ condition x)

firstWithNDigits n = snd $ firstThat (\(fib, ord) -> fib >= 10 ^ (n-1)) fibWithOrdinal