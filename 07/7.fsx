#load "../Common.fsx"

open System.IO
open Common

type Colour = Colour of string

type Rule = {
    Colour: Colour
    Contains: (Colour * int) seq
}

let parseRule (input: string): Rule =
    let parseColourAndCount (input: string): Colour * int =
        match input with
        | ParseRegex @"(\d+) (.*) bags?" [ Int count; colour ] -> Colour colour, count
        | _ -> failwith ("Invalid colour and count input: " + input)

    match input with
    | ParseRegex @"(.*) bags contain (.*)\." [ bagColour; contains ] ->
        match contains with
        | "no other bags" -> { Colour = Colour bagColour; Contains = Seq.empty; }
        | _ -> { Colour = Colour bagColour; Contains = (splitString ", " contains) |> Seq.map parseColourAndCount }
    | _ -> failwith ("Invalid input: " + input)

let rules =
    File.ReadLines "./input.txt"
    |> Seq.map parseRule
    |> Seq.map (fun rule -> (rule.Colour, rule))
    |> Map.ofSeq

let numberOfBagsWhichCanContain (colour: Colour): int =
    let rec coloursContainedByBag (rule: Rule): Colour seq =
        rule.Contains
        |> Seq.map fst
        |> Seq.map (fun colour -> (Seq.singleton colour) |> Seq.append (coloursContainedByBag rules.[colour]))
        |> Seq.concat
        |> Seq.distinct

    let bagsWhichCanContainColour =
        rules
        |> Map.map (fun key rule -> coloursContainedByBag rule)
        |> Map.filter (fun key containedColours -> Seq.contains colour containedColours)

    Seq.length bagsWhichCanContainColour

let myBagColour = Colour "shiny gold"

printf "Part 1: %d\r\n" (numberOfBagsWhichCanContain myBagColour)

let rec numberOfBagsContainedIn (colour: Colour): int =
    let containedBags =
        rules.[colour].Contains
        |> Seq.map (fun (containedColour, count) -> count * (numberOfBagsContainedIn containedColour))
        |> Seq.sum

    containedBags + 1

printf "Part 2: %d" ((numberOfBagsContainedIn myBagColour) - 1)
