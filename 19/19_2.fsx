#load "../Common.fsx"

open System
open System.IO
open Common
open FSharp.Collections

let parseRule (input: string): int * string =
    match input with
    | ParseRegex @"(\d+): (.*)" [ Int index; rule ] -> index, rule
    | _ -> failwith "Invalid rule input"

let rules: Map<int, string> =
    File.ReadAllText "./input.txt"
    |> splitString (Environment.NewLine + Environment.NewLine)
    |> Array.head
    |> splitString Environment.NewLine
    |> Seq.map parseRule
    |> Map.ofSeq

let messages: string array =
    File.ReadAllText "./input.txt"
    |> splitString (Environment.NewLine + Environment.NewLine)
    |> Array.item 1
    |> splitString Environment.NewLine

let rec cartesianProduct (lists: 'a list list): 'a list seq =
    match lists with
    | [] -> Seq.singleton []
    | head :: tail -> seq {for x in head do for xs in cartesianProduct tail -> x::xs}

type Rule =
    | Single of string
    | Repeated of string

let rec evaluateRule (index: int): string seq =
    let rule = rules.[index]
    let parts = splitString " | " rule

    let evaluatePart (part: string): string seq =
        match part with
        | ParseRegex @"""(.*)""" [ value ] -> Seq.singleton value
        | _ ->
            splitString " " part
            |> Seq.map (fun ruleIndex -> evaluateRule (int ruleIndex) |> Seq.toList)
            |> Seq.toList
            |> cartesianProduct
            |> Seq.map (String.concat "")

    parts |> Array.toList |> Seq.collect evaluatePart

let rule42 = evaluateRule 42
let rule31 = evaluateRule 31

let isValid (value: string) =
    // This solution relies on rule 0 being 8, then 11
    // Rule 8 evaluates as a sequence of Rule 42
    // Rule 11 evaluates as a sequence of Rule 42, then the same length sequence of Rule 31
    // No other rules reference rule 0, 8 or 11, so we can just special case these rules and manually check that we have the correct sequence of rule 42 then 31
    // This boils down to the string consisting of values from rule 42, then values from rule 31. We need more rule 42 occurrences than rule 31 (to ensure that rule 8 actually matches something)

    let rec consumeRule (rule: string seq) (value: string) (timesConsumed: int): string * int =
        let matchingRule = rule |> Seq.tryFind value.StartsWith
        match matchingRule with
        | Some matchingRule -> consumeRule rule (value.[matchingRule.Length..]) (timesConsumed + 1)
        | None -> value, timesConsumed

    let rule42Consumed, rule42Occurrences = consumeRule rule42 value 0
    let rule31Consumed, rule31Occurrences = consumeRule rule31 rule42Consumed 0

    match (rule31Consumed, rule42Occurrences, rule31Occurrences) with
    | "", rule42Occurrences, rule31Occurrences -> rule42Occurrences > rule31Occurrences && rule31Occurrences > 0
    | _ -> false

let validMessages =
    messages
    |> Seq.filter isValid

printf "%d" (validMessages |> Seq.length)

