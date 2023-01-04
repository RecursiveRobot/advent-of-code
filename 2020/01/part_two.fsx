open System.IO

let values = 
    System.IO.File.ReadAllLines("./01/input.txt")
    |> Array.map int

let allCombinations = seq {
    for a in values do
        for b in values do
            for c in values do
                if a <> b  && b <> c && a <> c then
                    yield (a, b, c)
}

let (a, b, c) = 
    allCombinations
    |> Seq.find (fun (a, b, c) -> a + b + c = 2020)

a * b * c