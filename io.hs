-- import Data.Char

-- main = do
--     putStrLn "Hello, world"
--     name <- getLine
--     let firstName = map toUpper (getFirstName name)
--         lastName = map toUpper (getLastName name)

--     putStrLn ("Hey " ++ firstName ++ " " ++ lastName ++ ", you rock!")

-- getFirstName = head . words
-- getLastName = last . words

main = do
    line <- getLine
    if null line
        then return ()
        else do
            putStrLn $ reverseWords line
            main

reverseWords :: String -> String
reverseWords = unwords . map reverse . words
