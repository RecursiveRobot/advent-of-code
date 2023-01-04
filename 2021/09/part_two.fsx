open System

type Map = int[][]

let getNeighbours (map: Map) (x,y) =
    seq {
        if x > 0 then
            yield (x-1,y)
        if y > 0 then
            yield (x, y-1)
        if x < map.Length-1 then
            yield (x+1, y)
        if y < map[x].Length-1 then
            yield (x, y+1)
    }

let isLowPoint (map: Map) x y =
    let lowestNeighbour = getNeighbours map (x, y) |> Seq.map (fun (x,y) -> map[x][y]) |> Seq.min
    let ownHeight = map[x][y]

    ownHeight < lowestNeighbour

let rec searchBasin map xy basinCoords =
    let neighbours = getNeighbours map xy
    let unsearchedNeighbours = neighbours |> Seq.filter (fun (x,y) -> map[x][y] <> 9 && (Set.contains (x,y) basinCoords |> not)) |> Set
    let newBasinCoords = Set.add xy basinCoords
    if Set.isEmpty unsearchedNeighbours then
        newBasinCoords
    else
        let basinCoordsIncludingNeighbours = Set.union newBasinCoords unsearchedNeighbours
        Seq.fold (fun acc elem -> Set.union (searchBasin map elem acc) acc) basinCoordsIncludingNeighbours basinCoordsIncludingNeighbours

let map =
    IO.File.ReadAllLines("./09/input.txt")
    |> Array.map (fun s -> s.ToCharArray())
    |> Array.map (Array.map (fun c -> int c - int '0'))

map |>
Array.mapi (fun x r -> r |> Array.mapi (fun y _ -> if isLowPoint map x y then Some (x,y) else None))
|> Array.collect id
|> Array.filter Option.isSome
|> Array.map Option.get
|> Array.map (fun a -> searchBasin map a Set.empty)
|> Array.map Set.count
|> Array.sortDescending
|> Array.take 3
|> Array.fold (*) 1