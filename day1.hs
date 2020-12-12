import System.IO

main = do
  s <- readFile "input1.txt"
  print (doWork s)
  print (doWork2 s)

-- complexity n^2, maximum improvement to n^2 / 2
solveProblem x = head [y*z | y <- x, z <- x, y + z == 2020]

-- complexity n^3, maximum improvement who knows...
solveProblem2 x = head [y*z*w | y <- x, z <- x, w <- x, y + z + w == 2020]

-- stuff
stringToNumbers s = [read c::Int | c <- (lines s)]
doWork = solveProblem . stringToNumbers
doWork2 = solveProblem2 . stringToNumbers
