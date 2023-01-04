open System

let getOpeningChar c =
    match c with
    | ')' -> '('
    | ']' -> '['
    | '}' -> '{'
    | '>' -> '<'
    | _ -> '_'

let getClosingChar c =
    match c with
    | '(' -> ')'
    | '[' -> ']'
    | '{' -> '}'
    | '<' -> '>'
    | _ -> '_'

let findCompletionCharacters chars =
    let rec findUnclosedCharacters stack chars =
        match chars with
        | [] -> Some stack
        | x :: xs when List.contains x ['(';'[';'{';'<'] -> findUnclosedCharacters (x :: stack) xs
        | x :: xs when List.contains x [')';']';'}';'>'] ->
            let y :: ys = stack
            if getOpeningChar x = y then
                findUnclosedCharacters ys xs
            else
                None
        | _ :: xs -> findUnclosedCharacters stack xs
    
    let unclosedChars = findUnclosedCharacters [] chars
    match unclosedChars with
    | None -> None
    | Some x -> List.map getClosingChar x |> Some

let getScore c =
    match c with
    | ')' -> 1
    | ']' -> 2
    | '}' -> 3
    | '>' -> 4
    | _ -> 0

let scoreAccumulator acc (elem: int) =
    (acc * bigint 5) + bigint elem

let sortedScores = 
    IO.File.ReadAllLines("./10/input.txt")
    |> Array.map (fun s -> s.ToCharArray())
    |> Array.map Array.toList
    |> Array.map findCompletionCharacters
    |> Array.filter Option.isSome
    |> Array.map Option.get
    |> Array.map (List.map getScore)
    |> Array.map (List.fold scoreAccumulator (bigint 0))
    |> Array.sortDescending

sortedScores[sortedScores.Length / 2]