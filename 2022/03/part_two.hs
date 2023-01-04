import Data.List (find)
import Data.Maybe (fromJust)
import GHC.Base (divInt)
import Data.Char (ord)

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = take n xs : chunk n (drop n xs)

parseLine :: String -> ([Char], [Char])
parseLine s = (take halfLength s, take halfLength . drop halfLength $ s)
    where halfLength = divInt (length s) 2

findDuplicateChar :: [String] -> Char
findDuplicateChar [a, b, c] = fromJust $ find (\x -> x `elem` b && x `elem` c) a

scoreChar :: Char -> Int
scoreChar a | a >= 'a' && a <= 'z' = ord a - ord 'a' + 1
scoreChar a | a >= 'A' && a <= 'Z' = ord a - ord 'A' + 27
scoreChar _ = 0

result :: IO Int
result = do
    input <- readFile "./03/input.txt"
    let chunks = chunk 3 (lines input)
    let scores = map (scoreChar . findDuplicateChar) chunks
    return (sum scores)