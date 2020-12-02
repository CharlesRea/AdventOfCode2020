open System.IO

let input = File.ReadLines("./1_input.txt") |> Seq.map int |> Seq.toList
let result =
    Seq.allPairs input input
    |> Seq.find (fun (x, y) -> x + y = 2020)
    |> (fun (x, y) -> x * y)

printf "%d" result
