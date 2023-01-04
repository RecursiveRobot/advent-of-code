#r "nuget: FSharp.FGL"

open FSharp.FGL.Undirected
open System

let edges =
    IO.File.ReadAllLines("./12/input.txt")
    |> Array.map (fun s -> s.Split("-"))

