

open System.IO
let preambleLength = 25

let input =
    File.ReadLines "./input.txt"
    |> Seq.map int64
    |> Seq.toArray

printf ""

let sumPairExists (values: int64 seq) (targetValue: int64): bool =
    Seq.allPairs values values
    |> Seq.filter (fun (x, y) -> x <> y)
    |> Seq.exists (fun (x, y) -> x + y = targetValue)

let validateIndex i: bool =
    let preamble = input.[i - preambleLength .. i - 1]
    sumPairExists preamble input.[i]

let invalidValue =
    let invalidIndex =
        seq { preambleLength .. input.Length - 1 }
        |> Seq.find (fun i -> not (validateIndex i))

    input.[invalidIndex]

printf "Part 1: %d\r\n" invalidValue


let contiguousNumbers =
    let findContiguousSum (startIndex: int): (int * int) option =
        seq { startIndex + 1 .. input.Length - 1 }
        |> Seq.tryFind (fun endIndex -> (Array.sum input.[startIndex..endIndex]) >= invalidValue)
        |> Option.bind (fun endIndex ->
            match Array.sum input.[startIndex..endIndex] = invalidValue with
            | true -> Some (startIndex, endIndex)
            | false -> None)

    let startIndex, endIndex = seq { 0 .. input.Length - 1 }
                               |> Seq.map findContiguousSum
                               |> Seq.find Option.isSome
                               |> Option.get

    input.[startIndex..endIndex]

let part2 = Array.max contiguousNumbers + Array.min contiguousNumbers
printf "Part 2: %d" part2
