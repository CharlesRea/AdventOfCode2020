open System
open System.IO

type Passport = {
    BirthYear: string option
    IssueYear: string option
    ExpirationYear: string option
    Height: string option
    HairColour: string option
    EyeColour: string option
    PassportId: string option
    CountryId: string option
}

let validatePassport (passport: Passport): bool =
    match passport with
    | { BirthYear = None }
    | { IssueYear = None }
    | { ExpirationYear = None }
    | { Height = None }
    | { HairColour = None }
    | { EyeColour = None }
    | { PassportId = None } -> false
    | _ -> true

let parsePassword (input: string): Passport =
    let inputRecords = input.Split(' ')
                       |> Seq.map (fun pair -> pair.Split([|":"; Environment.NewLine |], StringSplitOptions.None))
                       |> Seq.map (fun pair -> pair.[0], pair.[1])
                       |> Map.ofSeq
    {
        BirthYear = inputRecords |> Map.tryFind "byr"
        IssueYear = inputRecords |> Map.tryFind "iyr"
        ExpirationYear = inputRecords |> Map.tryFind "eyr"
        Height = inputRecords |> Map.tryFind "hgt"
        HairColour = inputRecords |> Map.tryFind "hcl"
        EyeColour = inputRecords |> Map.tryFind "ecl"
        PassportId = inputRecords |> Map.tryFind "pid"
        CountryId = inputRecords |> Map.tryFind "cid"
    }

let splitString (separator: string) (str: string): string[] =
    str.Split([|separator|], StringSplitOptions.None)

let passports: Passport seq =
    File.ReadAllText("./input.txt")
    |> splitString (Environment.NewLine + Environment.NewLine)
    |> Seq.map (fun str -> str.Replace(Environment.NewLine, " ").Trim())
    |> Seq.map parsePassword

let validPassports = passports
                     |> Seq.filter validatePassport
                     |> Seq.length


printf "%d" validPassports
