open System

let positions =
    IO.File.ReadAllText("./07/input.txt").Split(",")
    |> Array.map int

positions
|> Array.map (fun p -> (p, positions |> Array.sumBy (fun x -> abs (p - x))))
|> Array.minBy snd
|> snd