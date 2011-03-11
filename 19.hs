-- How many Sundays fell on the first of the month during the twentieth century (1 Jan 1901 to 31 Dec 2000)?

data Date = Date { day :: Int, month :: Int, year :: Int }
    deriving (Show, Eq)

nextDate :: Date -> Date
nextDate (Date day month year) | day < (daysPerMonth month year) - 1 = Date (day+1) month year
                               | month < 12                          = Date 1 (month+1) year
                               | otherwise                           = Date 1 1 (year+1)

daysPerMonth 2 year | (leapYear year) = 29
                    | otherwise       = 28
                    
daysPerMonth month year | month `elem` [1, 3, 5, 7, 8, 10, 12] = 31
                        | otherwise = 30
                        
leapYear year | year `mod` 4 /= 0   =  False
              | year `mod` 400 == 0 = True
              | year `mod` 100 == 0 = False
              | otherwise           = True
                        
                        
                     