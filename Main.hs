module Main(main) where 

import System.IO
import Data.List
import Data.Char (digitToInt)

main = do
    putStrLn "Welcome to Mastermind!"
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
    a <- [1..6]
    b <- [1..6]
    c <- [1..6]
    d <- [1..6]
    [0,0,0,0] : [[a,b,c,d]]


getPossibleCodes :: [[Int]] -> Int -> Int -> [[Int]]
getPossibleCodes (x:xs) red white = do 
    let currentIt = (maximum x)
    [y | y <- xs, (getCount y currentIt) == red + white, (numOfmatches y x) == red]

numOfmatches :: [Int] -> [Int] -> Int
numOfmatches x y = do 
    let q = if x !! 0 == y !! 0 then 1 else 0
    let w = if x !! 1 == y !! 1 then 1 else 0
    let e = if x !! 2 == y !! 2 then 1 else 0
    let r = if x !! 3 == y !! 3 then 1 else 0
    q + w + e + r


getCount :: [Int] -> Int -> Int
getCount x y = length [z | z <- x, z <= y]
