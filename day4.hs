import System.IO

main = do
  s <- readFile "input4test.txt"
  let r = splitData (lines s) [[]]
  print $ parseRecords r

splitData [] records = records
splitData (x:xs) records
    | null x = splitData xs ([]:records)
    | otherwise = splitData xs ((x:(head records)):(tail records))


parseRecord s = count (map isValid (map stripString (groupStrings s)))

stripString = head . (split ':')
groupStrings = words . (foldr (++) "")

-- parseRecord s = map (head . (split ':')) (words s)
-- parseRecord s = map isValid (map (head . (split ':')) (words s))
parseRecords = map parseRecord

isValid field = elem field ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]
-- areValid = map isValid

split sep s = words [if c == sep then ' ' else c | c <- s]
count = sum . (map fromEnum)
