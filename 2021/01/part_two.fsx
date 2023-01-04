type Accumulator = { Count: int; Depths: int[] }

let getNewDepths (ds: int[]) d =
    if ds.Length = 3 then
        Array.append ds[1..] [|d|]
    else
        Array.append ds [|d|]

let isDeeper (ds: int[]) d =
    if ds.Length < 3 then
        false
    else
        Array.sum (getNewDepths ds d) > Array.sum ds

let processRow acc d =
    match acc with
    | None -> 
        Some { Count = 0; Depths = [|d|] }
    | Some a when isDeeper a.Depths d -> 
        Some { Count = a.Count + 1; Depths = getNewDepths a.Depths d }
    | Some a -> 
        Some { a with Depths = getNewDepths a.Depths d }


let result = 
    System.IO.File.ReadAllLines("./01/input.txt")
    |> Array.map int
    |> Array.fold processRow None
    |> Option.map (fun a -> a.Count)
    |> Option.get