#load "../Common.fsx"

open System
open System.IO
open Common

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

let validateBirthYear (year: string option) =
    match year with
    | Some (Int year) when year >= 1920 && year <= 2002 -> true
    | _ -> false

let validateIssueYear (year: string option) =
    match year with
    | Some (Int year) when year >= 2010 && year <= 2020 -> true
    | _ -> false

let validateExpirationYear (year: string option) =
    match year with
    | Some (Int year) when year >= 2020 && year <= 2030 -> true
    | _ -> false

let validateHeight (height: string option) =
    match height with
    | Some (ParseRegex @"(\d*)cm" [ Int cm ]) when cm >= 150 && cm <= 193 -> true
    | Some (ParseRegex @"(\d*)in" [ Int inches ]) when inches >= 59 && inches <= 76 -> true
    | _ -> false

let validateHairColour (colour: string option) =
    match colour with
    | Some (ParseRegex "#[a-f0-9]{6}" _) -> true
    | _ -> false

let validateEyeColour (colour: string option) =
    match colour with
    | Some "amb"
    | Some "blu"
    | Some "brn"
    | Some "gry"
    | Some "grn"
    | Some "hzl"
    | Some "oth" -> true
    | _ -> false

let validatePassportId (id: string option) =
    match id with
    | Some (ParseRegex "[0-9]{9}" _) -> true
    | _ -> false

let validatePassport (passport: Passport): bool =
    validateBirthYear passport.BirthYear &&
    validateIssueYear passport.IssueYear &&
    validateExpirationYear passport.ExpirationYear &&
    validateHeight passport.Height &&
    validateHairColour passport.HairColour &&
    validateEyeColour passport.EyeColour &&
    validatePassportId passport.PassportId

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
