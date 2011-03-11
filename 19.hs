-- How many Sundays fell on the first of the month during the twentieth century (1 Jan 1901 to 31 Dec 2000)?

data Date = Date { year :: Int, month :: Int, day :: Int }
    deriving (Show, Eq, Ord)
    
data DayOfWeek = Mon|Tue|Wed|Thu|Fri|Sat|Sun
    deriving (Show, Eq, Enum, Ord)
    
nextDate :: Date -> Date
nextDate (Date year month day ) | day < (daysPerMonth month year) - 1 = Date year month (day+1)
                                | month < 12                          = Date year (month+1) 1
                                | otherwise                           = Date (year+1) 1 1

daysPerMonth 2 year | (leapYear year) = 29
                    | otherwise       = 28
                    
daysPerMonth month year | month `elem` [1, 3, 5, 7, 8, 10, 12] = 31
                        | otherwise = 30
                        
leapYear year | year `mod` 4 /= 0   =  False
              | year `mod` 400 == 0 = True
              | year `mod` 100 == 0 = False
              | otherwise           = True
                        
dates = iterate nextDate $ Date 1900 1 1
datesWithDayOfWeek = zip dates daysOfWeek
    where 
        daysOfWeek = map toDayOfWeek [0..]    
        toDayOfWeek x = toEnum (x `mod` 7) :: DayOfWeek
        
matching = length $ filter sundayFirst $ takeWhile (< (Date 2001 1 1, Mon)) $ dropWhile (< (Date 1901 1 1, Mon)) datesWithDayOfWeek
    where sundayFirst (Date _ _ 1, Sun) = True
          sundayFirst _                 = False




                     