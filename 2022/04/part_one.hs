import Data.Text (split, pack, unpack)
generateRange :: Enum a => a -> a -> [a]
generateRange a b = [a..b]

getRange :: String -> [Int]
getRange s = read <$> splitBy '-' s

rangesOverlapFully :: [Int] -> [Int] -> Bool
rangesOverlapFully [a,b] [c,d] = (a >= c && b <= d) || (c >= a && d <= b)

splitBy :: Char -> String -> [String]
splitBy c s = unpackPairs $ split (== c) $ pack s
    where unpackPairs ps = [unpack (ps !! 0), unpack (ps !! 1)]

processLine l = calculateOverlap $ getRange <$> splitBy ',' l
    where calculateOverlap ps = rangesOverlapFully (ps !! 0) (ps !! 1)

result :: IO Int
result = do
    input <- readFile "./04/input.txt"
    let overlappingLines = filter id $ processLine <$> lines input
    return (length overlappingLines)