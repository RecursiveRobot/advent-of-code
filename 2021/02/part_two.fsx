type Command = 
    | FORWARD of int 
    | UP of int 
    | DOWN of int

type Coordinates = {
    x: int;
    y: int;
    aim: int;
}

let parseCommand rt =
    match rt with
    | ("forward", v) -> FORWARD v
    | ("up", v) -> UP v
    | ("down", v) -> DOWN v
    | _ -> raise (System.ArgumentOutOfRangeException("Invalid command encountered."))

let processCommand coords command =
    match command with
    | FORWARD v -> { coords with x = coords.x + v ; y = coords.y + (coords.aim * v) }
    | UP v -> { coords with aim = coords.aim - v }
    | DOWN v -> { coords with aim = coords.aim + v }


let answer = 
    System.IO.File.ReadAllLines("./02/input.txt")
    |> Array.map (fun a -> a.Split(" "))
    |> Array.map (fun a -> (a[0], int a[1]))
    |> Array.map parseCommand
    |> Array.fold processCommand { x = 0; y = 0; aim = 0 }
    |> fun a -> a.x * a.y