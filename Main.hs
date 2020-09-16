module Main(main) where 

import System.IO
import Data.List
import Data.Char (digitToInt)

main = do
    putStrLn $ show $ (numOfmatches [3,4,4,4] [6,5,3,4])
    putStrLn $ show $ (numOfmatches [0, 5, 0, 3] [6,5,3,4])
    putStrLn $ show $ (numOfmatches [3,4,6,4] [6,5,3,4])
    putStrLn $ show $ (numOfmatches [4,3,5,5] [6,5,3,4])

    result <- makeGuess getAllPossibleCodes 0 0
    putStrLn $ show result

askForInput :: [[Int]] -> IO Bool
askForInput x = do
    putStrLn "How many correct matches?"
    red <- getLine
    let r = read red :: Int
    putStrLn "How many correct colours in wrong positions?"
    white <- getLine
    let w = read white :: Int
    if correctInput red white
        then if r == 4 
            then return True
        else
            makeGuess x r w
    else 
        return False

makeGuess :: [[Int]] -> Int -> Int -> IO Bool
makeGuess x red white = do
    putStrLn $ show (length x)
    let y = getPossibleCodes x red white
    putStrLn $ show (length y)

    putStrLn $ show (y !! 0)
    askForInput y

correctInput :: String -> String -> Bool
correctInput x y  
    | (length x /= 1) || (length y /= 1) = False
    | (digitToInt (x !! 0) + digitToInt (y !! 0) > 4) = False
    | (digitToInt (x !! 0) < 0 || digitToInt (y !! 0) < 0) = False
    | otherwise = True

data Code = Code {
    code :: [Int],
    possible :: Bool 
}

getAllPossibleCodes :: [[Int]]
getAllPossibleCodes = do
    a <- [0..6]
    b <- [0..6]
    c <- [0..6]
    d <- [0..6]
    [[a,b,c,d]]


getPossibleCodes :: [[Int]] -> Int -> Int -> [[Int]]
getPossibleCodes (x:xs) red white = do 
    let currentIt = (maximum x)
    [y | y <- xs, (getCount y currentIt) == red + white, (numOfmatches y x) == red]
    --if (red == 0)
    --else [y|  y <- xs, (getCount y currentIt) == red + white]

numOfmatches :: [Int] -> [Int] -> Int
--numOfmatches x y = length [z | z <- x, x !! 0 == y !! 0 || x !! 1 == y !! 1 || x !! 2 == y !! 2 || x !! 3 == y !! 3 ]
numOfmatches x y = do 
    let q = if x !! 0 == y !! 0 then 1 else 0
    let w = if x !! 1 == y !! 1 then 1 else 0
    let e = if x !! 2 == y !! 2 then 1 else 0
    let r = if x !! 3 == y !! 3 then 1 else 0
    q + w + e + r


getCount :: [Int] -> Int -> Int
getCount x y = length [z | z <- x, z <= y]
