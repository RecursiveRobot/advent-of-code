type Accumulator = { Count: int; Depth: int }

let processRow acc d =
    match acc with
    | None -> Some { Count = 0; Depth = d }
    | Some a when d > a.Depth -> Some { Count = a.Count + 1; Depth = d }
    | Some a -> Some { a with Depth = d }


let result = 
    System.IO.File.ReadAllLines("./01/input.txt")
    |> Array.map int
    |> Array.fold processRow None
    |> Option.map (fun a -> a.Count)
    |> Option.get