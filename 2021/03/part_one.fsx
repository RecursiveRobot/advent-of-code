let mostCommon inputs =
    Array.sum(inputs) > Array.length(inputs) / 2

let leastCommon inputs =
    Array.sum(inputs) < Array.length(inputs) / 2

let processPosition f arr pos =
    let inputs = Array.map (fun (a: char[]) -> int a[pos] - int '0') arr
    if f inputs then
        1
    else
        0

let getValue f arr =
        [0..11]
        |> List.map (processPosition f arr)
        |> List.rev
        |> List.mapi (fun i b -> b * (pown 2 i))
        |> List.sum

let getAnswer arr =
    getValue mostCommon arr * getValue leastCommon arr

let answer = 
    System.IO.File.ReadAllLines("./03/input.txt")
    |> Array.map (fun s -> s.ToCharArray())
    |> getAnswer