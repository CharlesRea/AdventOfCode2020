#load "../Common.fsx"

open System.IO
open Common

let input = File.ReadAllLines "./input.txt"

type Incidence = {
    StartPoint: int64
    Frequency: int64
}

let buses: Incidence list =
    input.[1]
    |> splitString ","
    |> Seq.mapi (fun i x -> x, i)
    |> Seq.filter (fun (x, i) -> x <> "x")
    |> Seq.map (fun (x, i) -> { Frequency = int64 x; StartPoint = int64 -i })
    |> Seq.toList

let rec gcd (x: int64) (y: int64) = if y = 0L then abs x else gcd y (x % y)
let lcm (x: int64) (y: int64) = x * y / (gcd x y)

let findIncidences (a: Incidence) (b: Incidence): Incidence =
    let firstIntersection =
            Seq.initInfinite (fun i -> (int64 i) * a.Frequency + a.StartPoint)
            |> Seq.filter (fun occurrenceOfA -> (occurrenceOfA - b.StartPoint) % b.Frequency = 0L)
            |> Seq.head
    { StartPoint = firstIntersection; Frequency = lcm a.Frequency b.Frequency }

let incidenceOfAllBuses =
    List.fold findIncidences (List.head buses) (List.tail buses)

printf "%d" incidenceOfAllBuses.StartPoint

