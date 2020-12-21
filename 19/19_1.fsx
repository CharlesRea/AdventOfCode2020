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

let rule0 = evaluateRule 0 |> Set.ofSeq

let validMessages =
    messages
    |> Seq.filter (fun message -> Set.contains message rule0)

printf "%d" (validMessages |> Seq.length)