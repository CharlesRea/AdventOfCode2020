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

let routes = [| (1, 1); (3, 1); (5, 1); (7, 1); (1, 2); |]

let countTrees ((routeX, routeY): int * int) =
    let pathCoords = seq { 0 .. routeY .. input.Length } |> Seq.mapi (fun i y -> (routeX * i % inputWidth, y))

    pathCoords
    |>Seq.filter (fun coord -> Set.contains coord trees)
    |>Seq.length

let result = routes
             |> Seq.map countTrees
             |> Seq.reduce (fun x y -> x * y)

printf "%d" result
