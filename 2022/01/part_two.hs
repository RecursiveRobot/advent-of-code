import Data.List

result :: IO Int
result = do
    input <- readFile "./01/input.txt"
    let inputLines = lines input
    let inputGroups = groupBy (\x y -> x /= "" && y /= "") inputLines
    let sumGroups = map (sum . map read . filter (/= "")) inputGroups
    return (sum (take 3 (sortBy (flip compare) sumGroups)))