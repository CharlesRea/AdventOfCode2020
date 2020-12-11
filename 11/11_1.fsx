open System.IO

type Seat = Empty | Occupied

type SeatMap = Map<int * int, Seat>

let input = File.ReadLines "./input.txt" |> Seq.toList

let printSeats (seats: SeatMap): unit =
    for y in 0..input.Length do
        for x in 0..input.[0].Length do
            let seat =
               match (seats |> Map.tryFind (x, y)) with
               | Some Empty -> 'L'
               | Some Occupied -> '#'
               | None -> '.'

            printf "%c" seat

        printf "\r\n"
    printf "\r\n"

let seats: SeatMap =
    input
    |> Seq.mapi (fun y row -> row |> Seq.mapi (fun x cell -> (cell, x, y)))
    |> Seq.reduce Seq.append
    |> Seq.filter (fun (cell, _, _) -> cell = 'L')
    |> Seq.map (fun (_, x, y) -> (x, y), Empty)
    |> Map.ofSeq

let isSeatOccupied (seats: SeatMap) (position: int * int): bool =
    (seats |> Map.tryFind position) = Some Occupied

let adjacentOccupiedSeats (seats: SeatMap) ((seatX, seatY): int * int): int =
    Seq.allPairs (seq { -1 .. 1 }) (seq { -1 .. 1 })
    |> Seq.filter (fun (x, y) -> x <> 0 || y <> 0)
    |> Seq.map (fun (x, y) -> (seatX + x, seatY + y))
    |> Seq.filter (isSeatOccupied seats)
    |> Seq.length

let nextSeat (seats: SeatMap) (position: int * int) (seat: Seat): Seat =

    match seat, (adjacentOccupiedSeats seats position) with
    | Empty, 0 -> Occupied
    | Occupied, adjacentOccupied when adjacentOccupied >= 4 -> Empty
    | seat, _ -> seat

let rec findSteadyState (seats: SeatMap): SeatMap =
    let nextSeats = seats |> Map.map (nextSeat seats)
    if nextSeats = seats then nextSeats else findSteadyState nextSeats

let steadyState = findSteadyState seats
let occupiedSeatsInSteadyState =
    steadyState
    |> Map.filter (fun position seat -> seat = Occupied)
    |> Map.count


printf "Part 1: %d" occupiedSeatsInSteadyState