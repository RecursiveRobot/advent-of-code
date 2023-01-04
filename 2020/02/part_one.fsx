open System

type PasswordPolicy = {
    From: int
    To: int
    Character: char
}

let getCharacterCount c (w: string) =
    w.ToCharArray()
    |> Array.filter (fun a -> a = c)
    |> Array.length

let isValidPassword (pw, policy) =
    let cc = getCharacterCount policy.Character pw
    (policy.From <= cc) && (cc <= policy.To)

let parseLine (s: string) =
    let parts = s.Split(": ")
    let ruleParts = parts[0].Split(" ")
    let counts = ruleParts[0].Split("-")

    (parts[1], { From = int counts[0]; To = int counts[1]; Character = ruleParts[1][0] })

let answer = 
    IO.File.ReadAllLines("./02/input.txt")
    |> Array.map parseLine
    |> Array.filter isValidPassword
    |> Array.length