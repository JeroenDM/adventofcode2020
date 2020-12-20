
factorial :: Int -> Int
factorial n = product [1..n]

myfun c n
  | n == 0 = c
  | otherwise = myfun (succ c) (n - 1)

getit = myfun 'A'
