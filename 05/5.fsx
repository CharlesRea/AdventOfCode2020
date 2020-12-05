open System.IO
type BinaryPartition = Lower | Upper

type BoardingPass = {
    Rows: BinaryPartition list
    Columns: BinaryPartition list
}

let parseBoardingPass (input: string): BoardingPass =
    let parsePartition (char: char) =
        match char with
        | 'F' | 'L' -> Lower
        | 'B' | 'R' -> Upper
        | _ -> invalidArg "char" "Unrecognised value"

    { Rows = input.[0..6] |> Seq.map parsePartition |> Seq.toList
      Columns = input.[7..10] |> Seq.map parsePartition |> Seq.toList }

let average x y =
    (x + y) / 2

let boardingPassSeat (pass: BoardingPass): int * int =
    let rec partition (minRange: int) (maxRange: int) (partitions: BinaryPartition list): int =
        match partitions with
        | Lower :: tail -> partition minRange (average minRange maxRange) tail
        | Upper :: tail -> partition ((average minRange maxRange) + 1) maxRange tail
        | [] -> minRange

    (partition 0 127 pass.Rows), (partition 0 7 pass.Columns)

let seatId (x, y) = x * 8 + y

let seatIds =
    File.ReadLines("./input.txt")
    |> Seq.map parseBoardingPass
    |> Seq.map boardingPassSeat
    |> Seq.map seatId
    |> Seq.sort
    |> Seq.toArray

let minSeatId = Array.head seatIds
let maxSeatId = Array.last seatIds

let missingSeatId =
   let seatIdsSet = seatIds |> Set.ofArray
   seq { minSeatId..maxSeatId } |> Seq.find (fun seatId -> not (Set.contains seatId seatIdsSet))

printf "Part 1 - highest seat ID: %A\r\n" (maxSeatId)
printf "Part 2 - my seat ID: %A\r\n" (missingSeatId)
