#load "../Common.fsx"

open System
open System.IO
open Common

type Ticket = int array

type Rule = {
    field: string
    values: int Set
}

type Input = {
    rules: Rule list
    myTicket: Ticket
    otherTickets: Ticket list
}

let parseRule (input: string): Rule =
    let parseRange (range: string): int seq =
        match range with
        | ParseRegex @"(\d+)-(\d+)" [ Int lower; Int upper ] -> seq { lower..upper }
        | _ -> failwith "Invalid range"

    match input with
    | ParseRegex @"(.+): (.+)" [ name; valuesString ] ->
        let values =
            valuesString
            |> splitString " or "
            |> Seq.map parseRange
            |> Seq.concat
            |> Set.ofSeq

        { field = name; values = values; }

    | _ -> failwith "Invalid input"

let parseTicket (input: string): Ticket =
    input |> splitString "," |> Seq.map int |> Seq.toArray

let parseInput (input: string): Input =
    let parts = input |> splitString "\r\n\r\n" |> Seq.toArray

    let rules = parts.[0] |> splitString "\r\n" |> Seq.map parseRule |> Seq.toList
    let myTicket = parts.[1].Replace("your ticket:\r\n", "") |> parseTicket
    let otherTickets = parts.[2].Replace("nearby tickets:\r\n", "") |> splitString "\r\n" |> Seq.map parseTicket |> Seq.toList

    { rules = rules; myTicket = myTicket; otherTickets = otherTickets; }

let input = parseInput (File.ReadAllText "./input.txt")
let allowedValues: int Set = input.rules |> Seq.map (fun rule -> rule.values) |> Set.unionMany
let isValid (value: int) = Set.contains value allowedValues
let isInvalid (value: int) =  not (isValid value)

let errorRate =
    let allTicketValues = input.otherTickets |> Seq.concat
    let invalidValues = allTicketValues |> Seq.filter isInvalid
    invalidValues |> Seq.sum

printf "Part 1: %d\n\n" errorRate

type Field = {
    values: int array
    myTicketValue: int
}

let validTickets =
    input.otherTickets
    |> Seq.append (Seq.singleton input.myTicket)
    |> Seq.filter (fun ticket -> ticket |> Seq.filter isInvalid |> Seq.isEmpty)
    |> Seq.toArray

let fields =
    validTickets
    |> Seq.map (Seq.indexed)
    |> Seq.concat
    |> Seq.groupBy fst
    |> Seq.map (fun (index, values) ->
        { values =  Seq.map snd values |> Seq.toArray
          myTicketValue = input.myTicket.[index] })
    |> Seq.toList

let rec findTicketValues (rules: Rule list) (fields: Field list): (string * int) list =
    let valueValidForRule (rule: Rule) (value: int) =
        Set.contains value rule.values

    let validFields (rule: Rule): Field seq =
        fields
        |> Seq.filter (fun field -> all (valueValidForRule rule) field.values)

    match rules with
        | [] -> List.empty
        | _ ->
            let fieldsForRules = rules |> Seq.map (fun rule -> (rule, validFields rule))
            let ruleWithSingleValidField = fieldsForRules |> Seq.tryFind (fun (rule, fields) -> Seq.length fields = 1)

            match ruleWithSingleValidField with
            | Some (rule, validFields) ->
                let field = Seq.exactlyOne validFields
                (rule.field, field.myTicketValue) :: findTicketValues (rules |> List.except [ rule ]) (fields |> List.except [field])
            | _ -> failwith "No matching rule found"

let ticketValues = findTicketValues input.rules fields
printf "Ticket values:\n"
printSequence ticketValues
printf "\n\n"

let productOfDepartureFields =
    ticketValues
    |> Seq.filter (fun (field, value) -> field.StartsWith("departure"))
    |> Seq.map (snd >> int64)
    |> Seq.reduce (fun x y -> x * y)

printf "Part 2: %d" productOfDepartureFields
