open System

let trees =
    IO.File.ReadAllLines("./03/input.txt")
    |> Array.map (fun s -> [|1..50|] |> Array.map (fun _ -> s.ToCharArray()) |> Array.collect id |> Array.map (fun c -> c = '#'))

let path = seq {
    for row in 0 .. trees.Length - 1 do
        yield trees[row][row*3]
}

let answer =
    path
    |> Seq.filter (fun a -> a = true)
    |> Seq.length