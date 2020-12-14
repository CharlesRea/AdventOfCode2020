open System
#load "../Common.fsx"

open System.IO
open Common

let toBinaryString (x: int64) = Convert.ToString(x, 2)

type Bitmask = { onMask: int64; floatingMasks: (int64 * int64) list }

type Command =
    | SetBitmask of char list
    | SetMemory of int64 * int64

let parseCommand (input: string): Command =
    match input with
    | ParseRegex @"mask = (.+)" [ mask ] -> SetBitmask (mask |> Seq.rev |> List.ofSeq)
    | ParseRegex @"mem\[(\d+)\] = (\d+)" [ Int64 address; Int64 value; ] -> SetMemory (address, value)
    | _ -> failwith "Unrecognised input"

let rec applyMask (value: int64) (mask: char list): int64 list =
    let rec processCharacter (mask: char list) (index: int) (value: int64): int64 seq =
        match mask with
        | head :: tail ->
            let nextValues =
                match head with
                | '1' -> Seq.singleton (value ||| (1L <<< index))
                | '0' -> Seq.singleton value
                | 'X' -> [ value ||| (1L <<< index); value &&& ~~~(1L <<< index) ] |> List.toSeq
                | _ -> failwith "Invalid mask character"
            nextValues |> Seq.map (processCharacter tail (index + 1)) |> Seq.concat
        | [] -> Seq.singleton value

    processCharacter mask 0 value |> Seq.toList

let commands =
    File.ReadLines "./input.txt"
    |> Seq.map parseCommand

type State = {
    memory: Map<int64, int64>
    bitmask: char list
}

let initialState = { memory = Map.empty; bitmask = List.empty }

let runCommand (state: State) (command: Command): State =
    match command with
    | SetBitmask bitmask -> { state with bitmask = bitmask }
    | SetMemory (address, value) ->
        let addresses = applyMask address state.bitmask
        let newMemory = addresses |> Seq.fold (fun memory address -> Map.add address value memory) state.memory
        { state with memory = newMemory }

let finalState =
    commands |> Seq.fold runCommand initialState

printf "Part 2: %d" (finalState.memory |> Map.toSeq |> Seq.map snd |> Seq.sum)