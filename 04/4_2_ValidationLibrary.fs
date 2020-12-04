open System.Text.RegularExpressions

open System
open System.IO
open AccidentalFish.FSharp.Validation

type Passport = {
    BirthYear: int option
    IssueYear: int option
    ExpirationYear: int option
    Height: string option
    HairColour: string option
    EyeColour: string option
    PassportId: string option
    CountryId: string option
}

let tryParseWith (tryParseFunc: string -> bool * _) = tryParseFunc >> function
    | true, v    -> Some v
    | false, _   -> None

let parseInt = tryParseWith System.Int32.TryParse
let parseDouble = tryParseWith System.Double.TryParse

let (|Int|_|)    = parseInt
let (|Double|_|) = parseDouble

let (|ParseRegex|_|) regex str =
   let m = Regex(regex).Match(str)
   if m.Success
   then Some (List.tail [ for x in m.Groups -> x.Value ])
   else None


let validateHeight (height: string) =
    match height with
    | ParseRegex @"(\d*)cm" [ Int cm ] when cm >= 150 && cm <= 193 -> Ok
    | ParseRegex @"(\d*)in" [ Int inches ] when inches >= 59 && inches <= 76 -> Ok
    | _ -> Errors [{ errorCode="height" ; message="height"; property = "height" }]

let matchesRegex (pattern: string) (propName: string) (value: string) : ValidationState =
    if Regex.IsMatch(value, pattern) then Ok else Errors [{ errorCode=propName ; message=propName; property = propName }]

let isOneOf (values: 'a seq) (propName: string) (value: 'a): ValidationState =
    if (Seq.contains value values) then Ok else Errors [{ errorCode=propName ; message=propName; property = propName }]

let validatePassport = createValidatorFor<Passport>() {
    validateRequired (fun o -> o.BirthYear) [
        isGreaterThanOrEqualTo 1920
        isLessThanOrEqualTo 2002
    ]
    validateRequired (fun o -> o.IssueYear) [
        isGreaterThanOrEqualTo 2010
        isLessThanOrEqualTo 2020
    ]
    validateRequired (fun o -> o.ExpirationYear) [
        isGreaterThanOrEqualTo 2020
        isLessThanOrEqualTo 2030
    ]
    validateRequired (fun o -> o.Height) [
        withFunction validateHeight
    ]
    validateRequired (fun o -> o.HairColour) [
        matchesRegex "#[a-f0-9]{6}"
    ]
    validateRequired (fun o -> o.EyeColour) [
        isOneOf [ "amb"; "blu"; "brn"; "gry"; "hzl"; "oth"; ]
    ]
    validateRequired (fun o -> o.PassportId) [
        matchesRegex "[0-9]{9}"
    ]
}

let parsePassword (input: string): Passport =
    let inputRecords = input.Split(' ')
                       |> Seq.map (fun pair -> pair.Split([|":"; Environment.NewLine |], StringSplitOptions.None))
                       |> Seq.map (fun pair -> pair.[0], pair.[1])
                       |> Map.ofSeq
    {
        BirthYear = inputRecords |> Map.tryFind "byr" |> Option.map int
        IssueYear = inputRecords |> Map.tryFind "iyr" |> Option.map int
        ExpirationYear = inputRecords |> Map.tryFind "eyr" |> Option.map int
        Height = inputRecords |> Map.tryFind "hgt"
        HairColour = inputRecords |> Map.tryFind "hcl"
        EyeColour = inputRecords |> Map.tryFind "ecl"
        PassportId = inputRecords |> Map.tryFind "pid"
        CountryId = inputRecords |> Map.tryFind "cid"
    }

let splitString (separator: string) (str: string): string[] =
    str.Split([|separator|], StringSplitOptions.None)

let passports: Passport seq =
    File.ReadAllText("../../../04/input.txt")
    |> splitString (Environment.NewLine + Environment.NewLine)
    |> Seq.map (fun str -> str.Replace(Environment.NewLine, " ").Trim())
    |> Seq.map parsePassword

let validPassports = passports
                     |> Seq.map validatePassport
                     |> Seq.filter (function Ok -> true | Errors _ -> false)
                     |> Seq.length


printf "%d" validPassports
