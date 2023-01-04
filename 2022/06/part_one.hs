import Data.List
import Data.Maybe

isMarker :: String -> Bool
isMarker s = groupLength == originalLength
    where
        groupLength = length $ group $ sort s
        originalLength = length s

getPrefixesOfLength :: Int -> String -> [String]
getPrefixesOfLength n s = [(take n . drop i) s | i <- [0..length s-n]]

main :: IO Int
main = do
    input <- readFile "./06/input.txt"
    let line = input
    let answer = findIndex isMarker $ getPrefixesOfLength 4 line

    return (fromJust answer + 4)
