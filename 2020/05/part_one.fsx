open System

type Range = int * int

let getRow (s: string) =
    let rec getRow' bs (min, max) =
        match bs with
        | [] -> max
        | h :: t when h = 'F' -> getRow' t (min, min + ((max-min) / 2))
        | h :: t when h = 'B' -> getRow' t (min + ((max-min) / 2), max)
        | _ -> raise (Exception("Unexpected row input."))
    
    getRow' (s.ToCharArray() |> Array.toList) (0, 127)

let getColumn (s: string) =
    let rec getColumn' bs (min, max) =
        match bs with
        | [] -> max
        | h :: t when h = 'L' -> getColumn' t (min, min + ((max-min) / 2))
        | h :: t when h = 'R' -> getColumn' t (min + ((max-min) / 2), max)
        | _ -> raise (Exception("Unexpected row input."))
    
    getColumn' (s.ToCharArray() |> Array.toList) (0, 7)

let getSeatId (s: string) =
    let row = getRow s[0..6]
    let column = getColumn s[7..10]

    (row * 8) + column

IO.File.ReadAllLines("./05/input.txt")
|> Array.map getSeatId
|> Array.max