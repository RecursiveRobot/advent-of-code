data Symbol = Rock | Paper | Scissors
    deriving (Eq, Show)

data Outcome = Win | Lose | Draw
    deriving (Eq)

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

getRequiredSymbol :: Symbol -> Outcome -> Symbol
getRequiredSymbol Rock Win = Paper
getRequiredSymbol Paper Win = Scissors
getRequiredSymbol Scissors Win = Rock
getRequiredSymbol a Draw = a
getRequiredSymbol Rock Lose = Scissors
getRequiredSymbol Paper Lose = Rock
getRequiredSymbol Scissors Lose = Paper

parseSymbol :: String -> Symbol
parseSymbol "A" = Rock
parseSymbol "B" = Paper
parseSymbol "C" = Scissors
parseSymbol _ = error "Invalid character."

parseOutcome :: String -> Outcome
parseOutcome "X" = Lose
parseOutcome "Y" = Draw
parseOutcome "Z" = Win
parseOutcome _ = error "Invalid character."

processLine :: String -> (Symbol, Symbol)
processLine = (\[a, b] -> (parseSymbol a, getRequiredSymbol (parseSymbol a) (parseOutcome b))) . words

processLines :: [String] -> [(Symbol, Symbol)]
processLines = map processLine . filter (/= "")

result :: IO Int
result = do
    input <- readFile "./02/input.txt"
    let pairs = processLines $ lines input
    let result = foldl (\acc (a,b) -> acc + getRoundScore a b) 0 pairs
    return result