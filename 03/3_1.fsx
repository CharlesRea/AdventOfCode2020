open System.Collections.Generic
open System.IO

let input = File.ReadLines("./input.txt") |> Seq.toList
let inputWidth = input.[0].Length

let trees =
    input
    |> Seq.mapi (fun y row -> row |> Seq.mapi (fun x cell -> (cell, x, y)))
    |> Seq.reduce Seq.append
    |> Seq.filter (fun (cell, _, _) -> cell = '#')
    |> Seq.map (fun (_, x, y) -> (x, y))
    |> Set.ofSeq


let pathCoords = seq { 0 .. input.Length } |> Seq.mapi (fun i y -> (3*i % inputWidth, y))

let numberTreesEncountered = pathCoords
                             |>Seq.filter (fun coord -> Set.contains coord trees)
                             |> Seq.length


printf "%d" numberTreesEncountered
