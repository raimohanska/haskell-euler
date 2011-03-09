-- Find the last ten digits of the series, 1^1 + 2^2 + 3^3 + ... + 1000^1000.

powerSum 1 = 1
powerSum n = n^n + powerSum (n-1)

last10DigitsOfPowerSum n = (powerSum n) `mod` 10000000000