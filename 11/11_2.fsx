open System.IO

type Seat = Empty | Occupied

type SeatMap = Map<int * int, Seat>

let input = File.ReadLines "./input.txt" |> Seq.toList

let rows = input.Length
let columns = input.[0].Length

let printSeats (seats: SeatMap): unit =
    for y in 0..rows - 1 do
        for x in 0..columns - 1 do
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

let isViewableSeatOccupied (seats: SeatMap) ((initialX, initialY): int * int) ((directionX, directionY): int * int): bool =
    let rec runStep ((x, y): int * int): bool =
        match x, y, (Map.tryFind (x, y) seats) with
        | x, _, _ when x < 0 || x >= columns -> false
        | _, y, _ when y < 0 || y >= rows -> false
        | _, _, (Some seat) ->
            seat = Occupied
        | x, y, None -> runStep (x + directionX, y + directionY)

    runStep (initialX + directionX, initialY + directionY)

let viewableOccupiedSeats (seats: SeatMap) ((seatX, seatY): int * int): int =
    Seq.allPairs (seq { -1 .. 1 }) (seq { -1 .. 1 })
    |> Seq.filter (fun (x, y) -> x <> 0 || y <> 0)
    |> Seq.filter (isViewableSeatOccupied seats (seatX, seatY))
    |> Seq.length

let nextSeat (seats: SeatMap) (position: int * int) (seat: Seat): Seat =

    match seat, (viewableOccupiedSeats seats position) with
    | Empty, 0 -> Occupied
    | Occupied, viewableOccupied when viewableOccupied >= 5 -> Empty
    | seat, _ -> seat

let rec findSteadyState (seats: SeatMap): SeatMap =
    let nextSeats = seats |> Map.map (nextSeat seats)
    if nextSeats = seats then nextSeats else findSteadyState nextSeats

let steadyState = findSteadyState seats
let occupiedSeatsInSteadyState =
    steadyState
    |> Map.filter (fun _ seat -> seat = Occupied)
    |> Map.count


printf "Part 2: %d" occupiedSeatsInSteadyState