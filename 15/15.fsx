#load "../Common.fsx"

open System
open System.IO
open Common

let input =
    File.ReadAllText "./input.txt"
    |> splitString ","
    |> Seq.map int
    |> Seq.toList

type State = {
    lastNumber: int
    ages: Map<int, int>
    lastSeenIndex: Map<int, int>
}

let nextState (state: State) (index: int) =
    if index % 100_000 = 0 then printf "Index: %i\n" index

    let nextValue =
        match index with
        | index when index < List.length input -> input.[index]
        | _ -> Map.tryFind state.lastNumber state.ages |> Option.defaultValue 0
    let nextAge = index - ((Map.tryFind nextValue state.lastSeenIndex) |> Option.defaultValue index)
    { state with
       lastNumber = nextValue
       ages = state.ages |> Map.add nextValue nextAge
       lastSeenIndex = state.lastSeenIndex |> Map.add nextValue index }

let initialState = { lastNumber = 0; ages = Map.empty; lastSeenIndex = Map.empty; }

let finalState =
    seq { 0..(30_000_000 - 1) } |> Seq.fold nextState initialState

printf "%d" finalState.lastNumber