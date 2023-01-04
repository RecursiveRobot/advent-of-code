open System

type Map = int[][]

let getNeighbours (map: Map) x y =
    seq {
        if x > 0 then
            yield map[x-1][y]
        if y > 0 then
            yield map[x][y-1]
        if x < map.Length-1 then
            yield map[x+1][y]
        if y < map[x].Length-1 then
            yield map[x][y+1]
    }

let isLowPoint (map: Map) x y =
    let lowestNeighbour = getNeighbours map x y |> Seq.min
    let ownHeight = map[x][y]

    ownHeight < lowestNeighbour

let map =
    IO.File.ReadAllLines("./09/input.txt")
    |> Array.map (fun s -> s.ToCharArray())
    |> Array.map (Array.map (fun c -> int c - int '0'))

map |>
Array.mapi (fun x r -> r |> Array.mapi (fun y _ -> if isLowPoint map x y then map[x][y] + 1 else 0))
|> Array.collect id
|> Array.sum