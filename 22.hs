-- What is the total of all the name scores in the file?
-- For example, when the list is sorted into alphabetical order, COLIN, which is worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list. So, COLIN would obtain a score of 938  53 = 49714.

main = do
    fileContent <- readFile "names.txt"    
    putStrLn $ show $ nameListScore $ parseNames fileContent
    return ()
    
parseNames = splitBy ',' . filter (/= '"')

splitBy delim []     = [""]
splitBy delim (c:cs) | c == delim = "" : rest
                     | otherwise  = (c : head rest) : tail rest
                     where rest = splitBy delim cs   
                     
nameListScore = sum . zipWith nameScore [1..]

nameScore pos name = pos * (sum $ map charScore name)

charScore :: Char -> Int
charScore c = fromEnum c - fromEnum 'A' + 1