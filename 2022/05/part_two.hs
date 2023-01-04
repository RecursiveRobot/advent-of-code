{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use head" #-}
import Data.Text (splitOn, pack, unpack)
import Text.Read (readMaybe)
import Data.Maybe (isJust, fromJust)
import Data.List

data Instruction = Instruction { count :: Int, from :: Int, to :: Int } deriving (Show)
type Stacks = [[Char]]

applyInstruction :: Instruction -> Stacks -> Stacks
applyInstruction (Instruction count from to) stacks =
    [ stack i | i <- [0..length stacks - 1]]
    where
        stack i | i == from = drop count (stacks !! i)
                | i == to = take count (stacks !! from) ++ stacks !! i
                | otherwise = stacks !! i

parseInstruction :: String -> Instruction
parseInstruction row = 
    Instruction { count = numbers !! 0, from = (numbers !! 1) - 1, to = (numbers !! 2) - 1 }
    where
        words = splitOn (pack " ") (pack row)
        numbers = fromJust <$> filter isJust (readMaybe . unpack <$> words) :: [Int]

parseStacks :: [String] -> Stacks
parseStacks input =
    [[ stackRows !! ri !! ci
        | ri <- [length stackRows - 1,length stackRows - 2..0], stackRows !! ri !! ci /= ' ']
        | ci <- [1,5..length (head stackRows) - 1]]
    where
        stackRows = reverse $ filter (elem '[') input

main :: IO String
main = do
    input <- readFile "./05/input.txt"
    let allRows = lines input
    let stackRows = takeWhile (/= "") allRows
    let instructionRows = filter (/= "") $ dropWhile (/= "") allRows

    let stacks = parseStacks stackRows
    let instructions = parseInstruction <$> instructionRows

    let finalStacks = foldl (flip applyInstruction) stacks instructions
    let answer = head <$> finalStacks
    return answer