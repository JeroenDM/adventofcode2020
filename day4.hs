import System.IO

main = do
  s <- readFile "input4test.txt"
  let r = splitData (lines s) []
--   print $ parseRecords r
  print $ lines s
--   print $ lines s
--   print s

splitData [] records = records
splitData (x:xs) records
    | null x = splitData xs records
    | otherwise = x:(splitData xs records)

parseRecord s = s
-- parseRecord s = map (head . (split ':')) (words s)
-- parseRecord s = map isValid (map (head . (split ':')) (words s))
parseRecords = map parseRecord

isValid field = elem field ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]
-- areValid = map isValid

split sep s = words [if c == sep then ' ' else c | c <- s]
