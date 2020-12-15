open System.Collections.Generic
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
    ages: Dictionary<int, int>
    lastSeenIndex: Dictionary<int, int>
}

let tryFind (key: int) (dict: Dictionary<int, int>) =
    if dict.ContainsKey(key) then Some(dict.[key]) else None

let add (key: int) (value: int) (dict: Dictionary<int, int>) =
    dict.Remove(key) |> ignore
    dict.Add(key, value)
    dict

let nextState (state: State) (index: int) =
    let nextValue =
        match index with
        | index when index < List.length input -> input.[index]
        | _ -> tryFind state.lastNumber state.ages |> Option.defaultValue 0

    let nextAge = index - ((tryFind nextValue state.lastSeenIndex) |> Option.defaultValue index)

    { state with
       lastNumber = nextValue
       ages = state.ages |> add nextValue nextAge
       lastSeenIndex = state.lastSeenIndex |> add nextValue index }

let initialState = { lastNumber = 0; ages = Dictionary<int, int>(); lastSeenIndex = Dictionary<int, int>(); }

let finalState =
    seq { 0..(30_000_000 - 1) } |> Seq.fold nextState initialState

printf "%d" finalState.lastNumber