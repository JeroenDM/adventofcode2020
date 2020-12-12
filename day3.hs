import System.IO

main = do
  s <- readFile "input3.txt"
  let rows = lines s
  let map = rowsToMap rows
  
  -- part 1
  print (countTrees map drive1)

  -- part 2
  let drivers = [(drive 1 1), (drive 1 3), (drive 1 5), (drive 1 7), (drive 2 1)]
  print (product [countTrees map driver | driver <- drivers])

countTrees map driver = (occurrence '#' (driver map 0 0 []))

rowsToMap rows = map cycle rows

lookAtMap map row col
  | row < (length map) = (map !! row) !! col
  | otherwise = 'X'

drive1 = (drive 1 3)

drive slope_row slope_col map row col path
  | row >= (length map) = path
  | otherwise = drive slope_row slope_col map (row + slope_row) (col + slope_col) (tile:path)
  where
    tile = lookAtMap map row col

occurrence letter s = length [c | c <- s, c == letter]
