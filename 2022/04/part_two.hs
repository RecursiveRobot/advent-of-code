import Data.Text (split, pack, unpack)
generateRange :: Enum a => a -> a -> [a]
generateRange a b = [a..b]

getRange :: String -> [Int]
getRange s = read <$> splitBy '-' s

rangesOverlap :: [Int] -> [Int] -> Bool
rangesOverlap [a,b] [c,d] = a <= d && b >= c

splitBy :: Char -> String -> [String]
splitBy c s = unpackPairs $ split (== c) $ pack s
    where unpackPairs ps = [unpack (ps !! 0), unpack (ps !! 1)]

processLine l = calculateOverlap $ getRange <$> splitBy ',' l
    where calculateOverlap ps = rangesOverlap (ps !! 0) (ps !! 1)

result :: IO Int
result = do
    input <- readFile "./04/input.txt"
    let overlappingLines = filter id $ processLine <$> lines input
    return (length overlappingLines)