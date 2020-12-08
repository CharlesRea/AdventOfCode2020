#load "../Common.fsx"

open System.IO
open Common


type Operation = Nop | Acc | Jmp
type Instruction = Operation * int

let (|Operation|_|) (str: string) =
   match str with
   | "nop" -> Some Nop
   | "acc" -> Some Acc
   | "jmp" -> Some Jmp
   | _ -> None

let parseInstruction (input: string): Instruction =
    match input with
        | ParseRegex @"(.*) (.*)" [ Operation operation; Int argument ] -> operation, argument
        | _ -> failwith ("Invalid instruction input: " + input)

let program =
    File.ReadLines("./input.txt")
    |> Seq.map parseInstruction
    |> Array.ofSeq

let part1Result: int =
    let rec runStep (accumulator: int) (location: int) (visitedLocations: Set<int>): int =
        match Set.contains location visitedLocations with
        | true -> accumulator
        | false ->
            let instruction = program.[location]
            let nextVisitedLocations = visitedLocations |> Set.add location
            match instruction with
            | Nop, _ -> runStep accumulator (location + 1) (nextVisitedLocations)
            | Acc, argument -> runStep (accumulator + argument) (location + 1) (nextVisitedLocations)
            | Jmp, argument -> runStep (accumulator) (location + argument) (nextVisitedLocations)

    runStep 0 0 Set.empty

printf "Part 1: %d\r\n" part1Result

let part22: int option =
    let runProgram (program: Instruction array): int option =
        let rec runStep (accumulator: int) (location: int) (visitedLocations: Set<int>): int option =
            match location >= program.Length with
            | true -> Some accumulator
            | false ->
                match Set.contains location visitedLocations with
                | true -> None
                | false ->
                    let instruction = program.[location]
                    let nextVisitedLocations = visitedLocations |> Set.add location
                    match instruction with
                    | Nop, _ -> runStep accumulator (location + 1) (nextVisitedLocations)
                    | Acc, argument -> runStep (accumulator + argument) (location + 1) (nextVisitedLocations)
                    | Jmp, argument -> runStep (accumulator) (location + argument) (nextVisitedLocations)

        runStep 0 0 Set.empty

    let operationIndices (operation: Operation): int array =
             program
             |> Array.mapi (fun index instruction -> index, instruction)
             |> Array.filter (fun (index, instruction) -> fst instruction = operation)
             |> Array.map (fun (index, _) -> index)

    let replaceOperation (operation: Operation) (index: int): Instruction array =
        let instruction = program.[index]
        replaceArrayElement index (operation, snd instruction) program

    let modifiedPrograms = Seq.append
                               (operationIndices Jmp |> Seq.map (fun i -> replaceOperation Nop i))
                               (operationIndices Nop |> Seq.map (fun i -> replaceOperation Jmp i))

    let results = modifiedPrograms |> Seq.map runProgram

    results |> Seq.find (fun result -> result.IsSome)


printf "Part 2: %A\r\n" part22
