open System.IO

type PasswordPolicy = {
    MinFrequency: int
    MaxFrequency: int
    Character: char
}

let parseRow (input: string): PasswordPolicy * string =
    let dashIndex = input.IndexOf('-')
    let spaceIndex = input.IndexOf(' ')
    let policy = {
        MinFrequency = input.[0..(dashIndex - 1)] |> int;
        MaxFrequency = input.[dashIndex + 1..spaceIndex - 1] |> int
        Character = input.[spaceIndex + 1]
    }
    policy, input.[(spaceIndex + 4)..]

let validate (policy: PasswordPolicy) (password: string): bool =
    let count = password
                |> Seq.filter (fun x -> x = policy.Character)
                |> Seq.length

    policy.MinFrequency <= count && count <= policy.MaxFrequency

let countValidPasswords =
    File.ReadLines("./2_input.txt")
    |> Seq.map parseRow
    |> Seq.filter (fun tuple -> tuple ||> validate)
    |> Seq.length

printf "%d" countValidPasswords
