open System.IO

let values = 
    System.IO.File.ReadLines("./01/input.txt")
    |> Seq.filter (fun a -> a <> "")
    |> Seq.map int
    |> Seq.toList

let (a, b) =
    values
    |> Seq.map (fun a -> (a, values |> List.tryFind (fun b -> a <> b && a + b = 2020)))
    |> Seq.filter (fun (_, b) -> (Option.isSome b))
    |> Seq.map (fun (a, b) -> (a, Option.get b))
    |> Seq.head

a * b