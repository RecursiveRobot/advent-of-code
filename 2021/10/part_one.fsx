open System

let getExpectedCharacter c =
    match c with
    | ')' -> '('
    | ']' -> '['
    | '}' -> '{'
    | '>' -> '<'
    | _ -> '_'

let rec findFirstSyntaxError stack chars =
    match chars with
    | [] -> None
    | x :: xs when List.contains x ['(';'[';'{';'<'] -> findFirstSyntaxError (x :: stack) xs
    | x :: xs when List.contains x [')';']';'}';'>'] ->
        let y :: ys = stack
        if getExpectedCharacter x = y then
            findFirstSyntaxError ys xs
        else
            Some x
    | _ :: xs -> findFirstSyntaxError stack xs

let getScore c =
    match c with
    | ')' -> 3
    | ']' -> 57
    | '}' -> 1197
    | '>' -> 25137
    | _ -> 0

IO.File.ReadAllLines("./10/input.txt")
|> Array.map (fun s -> s.ToCharArray())
|> Array.map Array.toList
|> Array.map (findFirstSyntaxError [])
|> Array.filter Option.isSome
|> Array.map Option.get
|> Array.map getScore
|> Array.sum