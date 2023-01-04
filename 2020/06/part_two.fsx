open System

let splitBy v list =
    let yieldRevNonEmpty list = 
        if list = [] then []
        else [List.rev list]

    let rec loop groupSoFar list = seq { 
        match list with
        | [] -> yield! yieldRevNonEmpty groupSoFar
        | head::tail when head = v ->
            yield! yieldRevNonEmpty groupSoFar
            yield! loop [] tail
        | head::tail ->
            yield! loop (head::groupSoFar) tail }
    
    loop [] list |> List.ofSeq

let answer = 
    IO.File.ReadAllLines("./06/input.txt")
    |> Array.toList
    |> splitBy ""
    |> List.map (List.map (fun s -> s.ToCharArray()))
    |> List.map (List.map Set)
    |> List.map (Set.intersectMany)
    |> List.map Seq.length
    |> List.sum