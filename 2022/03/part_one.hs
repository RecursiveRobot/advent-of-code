import Data.List (find)
import Data.Maybe (fromJust)
import GHC.Base (divInt)
import Data.Char (ord)

parseLine :: String -> ([Char], [Char])
parseLine s = (take halfLength s, take halfLength . drop halfLength $ s)
    where halfLength = divInt (length s) 2

findDuplicateChar :: ([Char], [Char]) -> Char
findDuplicateChar (a, b) = fromJust $ find (`elem` b) a

scoreChar :: Char -> Int
scoreChar a | a >= 'a' && a <= 'z' = ord a - ord 'a' + 1
scoreChar a | a >= 'A' && a <= 'Z' = ord a - ord 'A' + 27
scoreChar _ = 0

result :: IO Int
result = do
    input <- readFile "./03/input.txt"
    let scores = map (scoreChar .findDuplicateChar . parseLine) (lines input)
    return (sum scores)