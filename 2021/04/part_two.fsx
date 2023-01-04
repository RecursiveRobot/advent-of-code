open System

type Board = int[][]

let makeBoard (rows: string[]) =
    rows
    |> Array.map (fun r -> r.Split(" ", StringSplitOptions.RemoveEmptyEntries))
    |> Array.map (Array.map (int))

let getRows (board: Board) =
    [0..4]
    |> List.map (fun i -> board[i])

let getColumns (board: Board) =
    [0..4]
    |> List.map (fun i -> board |> Array.map (fun (row: int[]) -> row[i]))

let getRowsAndColumns (board: Board) =
    List.append (getRows board) (getColumns board)

let isWinningBoard numbers board =
    let winningRows = 
        List.append (getRows board) (getColumns board)
        |> List.filter (Array.forall (fun e -> List.contains e numbers))
    
    List.length winningRows > 0


let lines = System.IO.File.ReadAllLines("./04/input.txt")
let calledNumbers = 
    lines[0].Split(",") 
    |> Array.map (int) 
    |> Array.toList

let boards =
    lines
    |> Array.skip 1
    |> Seq.chunkBySize 6
    |> Seq.map (Array.skip 1)
    |> Seq.map makeBoard
    |> Seq.toList

let lastNonWinningNumbers =
    calledNumbers
    |> Seq.scan (fun acc elem -> elem :: acc) []
    |> Seq.skip 1
    |> Seq.takeWhile (fun cn -> (Seq.filter ((isWinningBoard cn) >> not) boards |> Seq.length) > 0)
    |> Seq.last

let lastBoardToWin =
    boards
    |> Seq.filter ((isWinningBoard lastNonWinningNumbers) >> not)
    |> Seq.head

let winningNumbers = calledNumbers |> List.take (lastNonWinningNumbers.Length + 1)

let unmarkedSum = 
    lastBoardToWin
    |> Array.collect id
    |> Set
    |> (fun a -> Set.difference a (Set winningNumbers))
    |> Seq.sum

let lastCalledNumber = Seq.last winningNumbers

let answer = unmarkedSum * lastCalledNumber