open System

type Point = { x: int; y: int}
type Vector = Point * Point

let getPointsCoveredByVector (v: Vector) =
    let (a, b) = v
    if a.x <> b.x && a.y <> b.y then
        seq {
            let distance = abs (a.x - b.x)
            let xd = if a.x > b.x then -1 else 1
            let yd = if a.y > b.y then -1 else 1

            for i in 0 .. distance ->
                { x = a.x + (i * xd); y = a.y + (i * yd) }
        }
    else
        seq {
            for x in min a.x b.x .. max a.x b.x do
                for y in min a.y b.y .. max a.y b.y ->
                    { x = x; y = y }
        }

let parseRow (r: string) =
    let points = 
        r.Split(" -> ")
        |> Array.map (fun s -> s.Split(","))
        |> Array.map (Array.map int)
        |> Array.map (fun p -> {x = p[0]; y = p[1]})
    
    (points[0], points[1])

let vectors =
    IO.File.ReadAllLines("./05/input.txt")
    |> Array.map parseRow
    |> Array.map getPointsCoveredByVector
    |> Seq.collect id
    |> Seq.groupBy id
    |> Seq.map (fun (k, v) -> (k, Seq.length v))
    |> Seq.filter (fun (k, l) -> l > 1)
    |> Seq.length