open System.IO

let input = File.ReadLines("./1_input.txt") |> Seq.map int |> Seq.toList
let result =
    Seq.allPairs input input
    |> Seq.allPairs input
    |> Seq.map (fun (x, (y, z)) -> (x, y, z))
    |> Seq.find (fun (x, y, z) -> x + y + z = 2020)
    |> (fun (x, y, z) -> x * y * z)

printf "%d" result
