open System

type PasswordPolicy = {
    Index1: int
    Index2: int
    Character: char
}

let getCharacterCount c (w: string) =
    w.ToCharArray()
    |> Array.filter (fun a -> a = c)
    |> Array.length

let isValidPassword (pw: string, policy) =
    (pw[policy.Index1] = policy.Character && pw[policy.Index2] <> policy.Character) || (pw[policy.Index2] = policy.Character && pw[policy.Index1] <> policy.Character)

let parseLine (s: string) =
    let parts = s.Split(": ")
    let ruleParts = parts[0].Split(" ")
    let counts = ruleParts[0].Split("-")

    (parts[1], { Index1 = int counts[0] - 1; Index2 = int counts[1] - 1; Character = ruleParts[1][0] })

let answer = 
    IO.File.ReadAllLines("./02/input.txt")
    |> Array.map parseLine
    |> Array.filter isValidPassword
    |> Array.length