#load "../Common.fsx"

open System.IO
open Common

let input = File.ReadAllLines "./input.txt"

type Occurrences = {
    StartPoint: int64
    Frequency: int64
}

let buses: Occurrences list =
    input.[1]
    |> splitString ","
    |> Seq.mapi (fun i x -> x, i)
    |> Seq.filter (fun (x, i) -> x <> "x")
    |> Seq.map (fun (x, i) -> { Frequency = int64 x; StartPoint = int64 -i })
    |> Seq.toList

let rec gcd (x: int64) (y: int64) = if y = 0L then abs x else gcd y (x % y)
let lcm (x: int64) (y: int64) = x * y / (gcd x y)

let allValues (occurrences: Occurrences): int64 seq =
    Seq.initInfinite (fun i -> (int64 i) * occurrences.Frequency + occurrences.StartPoint)

let containsValue (occurrences: Occurrences) (value: int64): bool =
    (value - occurrences.StartPoint) % occurrences.Frequency = 0L

let findIntersections (a: Occurrences) (b: Occurrences): Occurrences =
    {
        StartPoint = allValues a |> Seq.filter (b |> containsValue) |> Seq.head
        Frequency = lcm a.Frequency b.Frequency
    }

let occurrencesOfAllBuses =
    List.fold findIntersections (List.head buses) (List.tail buses)

printf "%d" occurrencesOfAllBuses.StartPoint
