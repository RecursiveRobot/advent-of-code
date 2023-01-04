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

let readValues (r: string) =
    r.Split(" ") 
    |> Array.filter (String.IsNullOrWhiteSpace >> not)
    |> Array.map (fun s -> s.Split(":"))
    |> Array.map (fun g -> (g[0], g[1]))

let isValid (arr: (string * string)[]) =
    let requiredFields = Set ["byr";"iyr";"eyr";"hgt";"hcl";"ecl";"pid"]
    let presentFields = 
        arr
        |> Array.map (fst)
        |> Set

    Seq.length (Set.intersect requiredFields presentFields) = 7

IO.File.ReadLines("./04/input.txt")
|> Seq.toList
|> splitBy ""
|> List.map (fun r -> String.Join(" ", r))
|> List.map readValues
|> List.filter isValid
|> List.length