
import System.IO
import Data.List (sort)

-- some test darta
d = "BFFFBBFRRR"
e = "BBFFBBFRLL"
a = [1..10]

main = do
  s <- readFile "input5.txt"
  let d = lines s
  let ids = map calcID d
  print $ maximum ids
  let ids_sorted = sort ids
  let place = findIndex (diff ids_sorted) 2 0
  print $ ids_sorted !! place
  print $ ids_sorted !! (place+1)

calcPlace n = (row n, col n)
calcID n = (row n) * 8 + (col n)

row n = workit (take 7 n) (0,128)
col n = workit (drop 7 n) (0, 8)

-- assumes the correct list length
workit (x:xs) (l, u)
    | (x == 'F') || (x == 'L') = workit xs (l, u - step)
    | (x == 'B') || (x == 'R') = workit xs (l + step, u)
    | otherwise = error "Wrong input for workit"
    where step = (u - l) / 2
workit x (l, u) = l

diff a = [ y - x | (x,y) <- zip (take (l-1) a) (drop 1 a)]
    where l = length a

findIndex [] value i = -1
findIndex (x:xs) value i
    | x == value = i
    | otherwise = findIndex xs value (i+1)
