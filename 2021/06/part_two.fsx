open System

// (Age, Count)
type Fish = int * bigint

let tickSingle (age, count) =
    match age with
    | 0 -> [|(6, count); (8, count)|]
    | a -> [|(a-1, count)|]

let rec tick maxGen gen (fish: Fish[]) =
    match gen with
    | g when g = maxGen -> fish
    | g ->
        let newFish = 
            fish 
            |> Array.map tickSingle
            |> Array.collect id
            |> Array.groupBy fst
            |> Array.map (fun (k, v) -> (k, v |> Array.sumBy snd))
        tick maxGen (g+1) newFish

let answer =
    IO.File.ReadAllText("./06/input.txt").Split(",")
    |> Array.map int
    |> Array.groupBy id
    |> Array.map (fun (k, v) -> (k, bigint v.Length))
    |> tick 256 0
    |> Array.sumBy snd