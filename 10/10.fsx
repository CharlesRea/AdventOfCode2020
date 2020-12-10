

open System.IO

let input =
    File.ReadLines "./input.txt"
    |> Seq.map int
    |> Seq.toList

let maxInput = List.max input

let adapters = input |> List.append (List.singleton (maxInput + 3)) |> List.sort

let joltageDifferences =
    adapters
    |> Seq.append (Seq.singleton 0)
    |> Seq.sort
    |> Seq.pairwise
    |> Seq.map (fun (x, y) -> y - x)
    |> Seq.groupBy (fun x -> x)
    |> Seq.map (fun (difference, values) -> (difference, Seq.length values))
    |> Map.ofSeq

printf "Differences: %A \r\n" joltageDifferences

let part1 = joltageDifferences.[1] * (joltageDifferences.[3])
printf "Part 1: %d\r\n\r\n" part1

let countAllValidPaths =
    let foldAdapter (pathsToAdapters: Map<int, int64>) (adapter: int): Map<int, int64> =
        let pathsToDifference (difference: int) =
            pathsToAdapters |> Map.tryFind (adapter - difference) |> Option.defaultValue 0L
        pathsToAdapters |> Map.add adapter (seq { 1..3 } |> Seq.sumBy pathsToDifference)

    let initialState = [(0, 1L)] |> Map.ofList

    Seq.fold foldAdapter initialState adapters |> Map.find maxInput

printf "Part 2: %d" countAllValidPaths