import System.IO

main = do
  s <- readFile "input2.txt"
  print (doWork s)
  print (doWork2 s)

-- the whole is made of ...
doWork = count . (map parseLine) . lines
doWork2 = count .(map parseLine2) . lines

-- .. some large parts ...
parseLine l = isInBounds (occurrence letter password) bounds
  where
    letter = parseChar l
    password = parsePassword l
    bounds = parseBounds l

parseBounds = (map toInt) . (split '-') . head . words
parseChar = head . head . tail .words  -- double heads to get Char from String
parsePassword = last . words

-- ... and some small parts
toInt c = read c::Int
split sep s = words [if c == sep then ' ' else c | c <- s]
occurrence letter s = length [c | c <- s, c == letter]
count = sum . (map fromEnum)
isInBounds x bounds = x >= (head bounds) && x <= (last bounds)

-- but I did not get to use this part :(
-- parseLine l = [f x | (f, x) <- zip [head, head, head] (words l)]


-- PART 2
parseLine2 l =  ((parseChar l) == (getLetter1 l)) /= ((parseChar l) == (getLetter2 l))
getLetter1 l =  (parsePassword l) !! (head (parseBounds l) - 1)
getLetter2 l =  (parsePassword l) !! (last (parseBounds l) - 1)
