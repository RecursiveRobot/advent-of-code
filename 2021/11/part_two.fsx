open System

let mutable board =
    IO.File.ReadAllLines("./11/input.txt")
    |> Array.map (fun s -> s.ToCharArray())
    |> Array.map (Array.map (fun c -> int c - int '0'))

let getNeighbours (x,y) =
    seq {
        if x > 0 && y > 0 then
            yield (x-1,y-1)
        if y > 0 then
            yield (x,y-1)
        if x < board.Length-1 && y > 0 then
            yield (x+1,y-1)
        if x > 0 then
            yield (x-1,y)
        if x < board.Length-1 then
            yield (x+1,y)
        if x > 0 && y < board[0].Length-1 then
            yield (x-1,y+1)
        if y < board[0].Length-1 then
            yield (x,y+1)
        if x < board.Length-1 && y < board[0].Length-1 then
            yield (x+1,y+1)
    }

let rec incrementCell (x,y) =
    board[x][y] <- (board[x][y] + 1)
    if board[x][y] = 10 then
        getNeighbours (x,y)
        |> Seq.iter incrementCell

let allCoords =
    seq {
        for x in 0..board.Length-1 do
            for y in 0..board[0].Length-1 ->
                (x,y)
    }

let tick _ =
    allCoords
    |> Seq.iter incrementCell
    |> ignore

    let flashCount =
        allCoords
        |> Seq.filter (fun (x,y) -> board[x][y] > 9)
        |> Seq.length

    allCoords
    |> Seq.iter (fun (x,y) -> if board[x][y] > 9 then board[x][y] <- 0)
    |> ignore

    flashCount

seq { 1 .. Int32.MaxValue }
|> Seq.find (fun i -> tick i = board.Length * board[0].Length)