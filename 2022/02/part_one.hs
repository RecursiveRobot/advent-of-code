data Symbol = Rock | Paper | Scissors
    deriving (Eq, Show)

getSymbolScore :: Symbol -> Int
getSymbolScore Rock = 1
getSymbolScore Paper = 2
getSymbolScore Scissors = 3

getRoundScore :: Symbol -> Symbol -> Int
getRoundScore a b
    | b == Scissors && a == Paper   = 6 + symbolScore
    | b == Paper && a == Rock       = 6 + symbolScore
    | b == Rock && a == Scissors    = 6 + symbolScore
    | a == b                        = 3 + symbolScore
    | otherwise                     = symbolScore
    where 
        symbolScore = getSymbolScore b

parseSymbol :: String -> Symbol
parseSymbol "A" = Rock
parseSymbol "X" = Rock
parseSymbol "B" = Paper
parseSymbol "Y" = Paper
parseSymbol "C" = Scissors
parseSymbol "Z" = Scissors
parseSymbol _ = error "Invalid character."

processLine :: String -> (Symbol, Symbol)
processLine = (\[a, b] -> (a, b)) . map parseSymbol . words

processLines :: [String] -> [(Symbol, Symbol)]
processLines = map processLine . filter (/= "")

result :: IO Int
result = do
    input <- readFile "./02/input.txt"
    let pairs = processLines $ lines input
    let result = foldl (\acc (a,b) -> acc + getRoundScore a b) 0 pairs
    return result