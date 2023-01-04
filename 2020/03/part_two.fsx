open System

let trees =
    IO.File.ReadAllLines("./03/input.txt")
    |> Array.map (fun s -> [|1..100|] |> Array.map (fun _ -> s.ToCharArray()) |> Array.collect id |> Array.map (fun c -> c = '#'))

let getPath x y =
    seq {
        for row in 0 .. y .. trees.Length - 1 do
            yield trees[row][int (float row * x)]
    }

let answer =
    [(1.0,1);(3.0,1);(5.0,1);(7.0,1);(0.5,2)]
    |> List.map (fun (x, y) -> getPath x y)
    |> List.map (Seq.filter (fun b -> b = true))
    |> List.map (Seq.length)
    |> List.map bigint
    |> List.fold (*) (bigint 1)