open System

type Input = string[] * string[]

let stringToSet (s: string) =
    s.ToCharArray() |> Set

let parseRow (r: string) =
    r.Split(" | ") |> (fun a -> (a[0].Split(" ") |> Array.map stringToSet, a[1].Split(" ")))

let getCharacters (ss : Set<char>[]) =
    let one = ss |> Seq.find (fun s -> s.Count = 2)
    let four = ss |> Array.find (fun s -> s.Count = 4)
    let seven = ss |> Array.find (fun s -> s.Count = 3)
    let eight = ss |> Array.find (fun s -> s.Count = 7)
    let zero_six_nine = ss |> Array.filter (fun s -> s.Count = 6)
    let two_three_five = ss |> Array.filter (fun s -> s.Count = 5)

    let a = Set.difference seven one
    let bd = Set.difference four one
    let abd = Set.union a bd

    let zero = Seq.find (Set.isSubset bd >> not) zero_six_nine

    let d = Set.difference eight zero
    let b = Set.difference bd d
    let cdf = Set.difference four b

    let five = Seq.find (Set.isSubset abd) two_three_five
    let three = Seq.find (Set.isSubset cdf) two_three_five
    let two = two_three_five |> Seq.find (fun a -> a <> five && a <> three)
    let nine = Seq.find (Set.isSubset cdf) zero_six_nine
    let six = zero_six_nine |> Seq.find (fun a -> a <> zero && a <> nine)

    Map [(one, 1); (two, 2); (three, 3); (four, 4); (five, 5); (six, 6); (seven, 7); (eight, 8); (nine, 9); (zero, 0)]

let getValues cm ss =
    ss
    |> Array.map stringToSet
    |> Array.map (fun s -> Map.find s cm)

let processRow (a, b) =
    let cm = getCharacters a
    getValues cm b
    |> Array.map string
    |> (fun s -> String.Join("", s))
    |> int

let answer =
    IO.File.ReadAllLines("./08/input.txt")
    |> Array.map parseRow
    |> Array.map processRow
    |> Array.sum