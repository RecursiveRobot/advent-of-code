open System

let tickSingle age =
    match age with
    | 0 -> [|6; 8|]
    | a -> [|a-1|]

let rec tick maxGen gen fish =
    match gen with
    | g when g = maxGen -> fish
    | g -> 
        let newFish = fish |> Array.map tickSingle |> Array.collect id
        tick maxGen (g+1) newFish

let answer =
    IO.File.ReadAllText("./06/input.txt").Split(",")
    |> Array.map int
    |> tick 80 0
    |> Array.length