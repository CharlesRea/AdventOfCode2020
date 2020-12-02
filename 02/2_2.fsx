open System.IO

type PasswordPolicy = {
    IndexOne: int
    IndexTwo: int
    Character: char
}

let parseRow (input: string): PasswordPolicy * string =
    let dashIndex = input.IndexOf('-')
    let spaceIndex = input.IndexOf(' ')
    let policy = {
        IndexOne = (input.[0..(dashIndex - 1)] |> int) - 1;
        IndexTwo = (input.[dashIndex + 1..spaceIndex - 1] |> int) - 1
        Character = input.[spaceIndex + 1]
    }
    policy, input.[(spaceIndex + 4)..]

let validate (policy: PasswordPolicy) (password: string): bool =
    let charOne = password.[policy.IndexOne]
    let charTwo = password.[policy.IndexTwo]
    charOne <> charTwo && (charOne = policy.Character || charTwo = policy.Character)

let countValidPasswords =
    File.ReadLines("./2_input.txt")
    |> Seq.map parseRow
    |> Seq.filter (fun tuple -> tuple ||> validate)
    |> Seq.length

printf "%d" countValidPasswords
