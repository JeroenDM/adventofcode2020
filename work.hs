
main = do
    let d = ["a", "a", "", "b", "b", "", "c"]
    -- print (splitData d [[]])
    print (foldr (++) "" d)

splitData [] records = records
splitData (x:xs) records
    | null x = splitData xs ([]:records)
    | otherwise = splitData xs ((x:(head records)):(tail records))

