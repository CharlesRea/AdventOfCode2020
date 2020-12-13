#load "../Common.fsx"

open System
open System.IO
open Common

let input = File.ReadAllLines "./input.txt"

let arrivalTime = input.[0] |> int
let busIds =
    input.[1]
    |> splitString ","
    |> Seq.filter (fun x -> x <> "x")
    |> Seq.map int
    |> Seq.toList

let (earliestBusId, earliestDepartureTime) =
    busIds
    |> Seq.map (fun busId -> busId, busId * ((arrivalTime / busId) + 1))
    |> Seq.minBy snd

printf "Part 1: Bus %d, departure time %d, solution %d" (earliestBusId) (earliestDepartureTime) ((earliestDepartureTime - arrivalTime) * earliestBusId)