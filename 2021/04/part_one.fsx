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

let winningNumbers =
    calledNumbers
    |> Seq.scan (fun acc elem -> elem :: acc) []
    |> Seq.skip 1
    |> Seq.find (fun cn -> Seq.tryFind (isWinningBoard cn) boards |> Option.isSome)

let winningBoard =
    boards
    |> Seq.find (isWinningBoard winningNumbers)

let unmarkedSum = 
    winningBoard
    |> Array.collect id
    |> Set
    |> (fun a -> Set.difference a (Set winningNumbers))
    |> Seq.sum

let lastCalledNumber = Seq.head winningNumbers

let answer = unmarkedSum * lastCalledNumber