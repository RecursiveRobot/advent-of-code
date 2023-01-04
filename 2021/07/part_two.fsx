open System

let positions =
    IO.File.ReadAllText("./07/input.txt").Split(",")
    |> Array.map int

[|Array.min positions .. Array.max positions|]
|> Array.map (fun p -> (p, positions |> Array.sumBy (fun x -> [1..abs (p - x)] |> List.sum)))
|> Array.minBy snd
|> snd