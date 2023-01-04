let mostCommon inputs =
    let oneCount = Array.sum inputs
    let zeroCount = Array.length inputs - oneCount

    if oneCount >= zeroCount then
        1
    else
        0

let leastCommon inputs =
    let oneCount = Array.sum inputs
    let zeroCount = Array.length inputs - oneCount

    if zeroCount <= oneCount then
        0
    else
        1

let getDecimalValue cs =
    cs
    |> Array.rev
    |> Array.mapi (fun i b -> b * (pown 2 i))
    |> Array.sum

let rec findValue f (arr: int[][]) p =
    match arr with
    | xs when xs.Length = 1 -> getDecimalValue xs[0]
    | xs ->
        let inputs = xs |> Array.map (fun a -> a[p])
        let filterValue = f inputs
        let filteredList = xs |> Array.filter (fun a -> a[p] = filterValue)
        findValue (f) filteredList (p+1)

let getAnswer (arr: int[][]) =
    findValue (mostCommon) arr 0 * findValue (leastCommon) arr 0

let answer = 
    System.IO.File.ReadAllLines("./03/input.txt")
    |> Array.map (fun s -> s.ToCharArray())
    |> Array.map (Array.map (fun c -> int c - int '0'))
    |> getAnswer