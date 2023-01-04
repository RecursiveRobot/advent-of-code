open System
open System.Text.RegularExpressions

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

let validateField k (s: string) =
    try
        match k with
        | "byr" ->
            let v = int s
            s.Length = 4 && 1920 <= v && v <= 2002
        | "iyr" ->
            let v = int s
            s.Length = 4 && 2010 <= v && v <= 2020
        | "eyr" ->
            let v = int s
            s.Length = 4 && 2020 <= v && v <= 2030
        | "hgt" ->
            let v = int s[..s.Length - 3]
            let symbol = s[s.Length - 2..]
            if symbol = "cm" then
                150 <= v && v <= 193
            else if symbol = "in" then
                59 <= v && v <= 76
            else
                false
        | "hcl" ->
            Regex.IsMatch(s, "^#[0-9A-Fa-f]{6}$")
        | "ecl" ->
            ["amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth"] |> List.contains s
        | "pid" ->
            Regex.IsMatch(s, "^[0-9]{9}$")
        | _ -> 
            true
    with _ ->
        printfn "Validation failed for: %A = %A" k s
        false

let isValid (arr: (string * string)[]) =
    let requiredFields = Set ["byr";"iyr";"eyr";"hgt";"hcl";"ecl";"pid"]
    let presentFields = arr |> Array.map (fst) |> Set
    let hasAllRequiredFields = Seq.length (Set.intersect requiredFields presentFields) = 7

    let satisfiesAllRules = arr |> Array.filter (fun (k, _) -> requiredFields.Contains(k)) |> Array.forall (fun (k, v) -> validateField k v)

    hasAllRequiredFields && satisfiesAllRules

IO.File.ReadLines("./04/input.txt")
|> Seq.toList
|> (splitBy "")
|> List.map (fun r -> String.Join(" ", r))
|> List.map readValues
|> List.filter isValid
|> List.length