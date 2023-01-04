open System

type Input = string[] * string[]

let parseRow (r: string) =
    r.Split(" | ") |> (fun a -> (a[0].Split(" "), a[1].Split(" ")))

let uniqueLengths = [2; 3; 4; 7]

let answer =
    IO.File.ReadAllLines("./08/input.txt")
    |> Array.map parseRow
    |> Array.map snd
    |> Array.collect id
    |> Array.map (fun s -> s.Length)
    |> Array.filter (fun l -> List.contains l uniqueLengths)
    |> Array.length