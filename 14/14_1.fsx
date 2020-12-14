open System
#load "../Common.fsx"

open System.IO
open Common

type Bitmask = { onMask: int64; offMask: int64 }

type Command =
    | SetBitmask of Bitmask
    | SetMemory of int64 * int64

let parseBitmask (mask: string): Bitmask =
    let onMask =
        mask
        |> Seq.rev
        |> indexed
        |> Seq.filter (fun (x, i) -> x = '1')
        |> Seq.fold (fun (mask: int64) (_, i) -> mask ||| (1L <<< i)) 0L

    let offMask =
        mask
        |> Seq.rev
        |> indexed
        |> Seq.filter (fun (x, i) -> x = '0')
        |> Seq.fold (fun (mask: int64) (_, i) -> mask &&& ~~~(1L <<< i)) (~~~0L)

    { onMask = onMask; offMask = offMask; }

let parseCommand (input: string): Command =
    match input with
    | ParseRegex @"mask = (.+)" [ mask ] -> SetBitmask (parseBitmask mask)
    | ParseRegex @"mem\[(\d+)\] = (\d+)" [ Int64 address; Int64 value; ] -> SetMemory (address, value)
    | _ -> failwith "Unrecognised input"

let commands =
    File.ReadLines "./input.txt"
    |> Seq.map parseCommand

let toBinaryString (x: int64) = Convert.ToString(x, 2)

type State = {
    memory: Map<int64, int64>
    bitmask: Bitmask
}

let initialState = { memory = Map.empty; bitmask = { onMask = 0L; offMask = ~~~0L; } }

let applyMask (value: int64) (mask: Bitmask): int64 =
    (value ||| mask.onMask) &&& mask.offMask

let runCommand (state: State) (command: Command): State =
    match command with
    | SetBitmask bitmask -> { state with bitmask = bitmask }
    | SetMemory (address, value) -> { state with memory = state.memory |> Map.add address (applyMask value state.bitmask) }

let finalState =
    commands |> Seq.fold runCommand initialState

printf "%A\n" finalState.memory
printf "Part 1: %d" (finalState.memory |> Map.toSeq |> Seq.map snd |> Seq.sum)