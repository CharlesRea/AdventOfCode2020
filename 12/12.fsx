#load "../Common.fsx"

open System
open System.IO
open Common

type Action =
    | North
    | South
    | East
    | West
    | Left
    | Right
    | Forward

let parseCommand (input: string): Action * int =
    match input with
    | ParseRegex @"N(\d+)" [ Int value ] -> North, value
    | ParseRegex @"S(\d+)" [ Int value ] -> South, value
    | ParseRegex @"E(\d+)" [ Int value ] -> East, value
    | ParseRegex @"W(\d+)" [ Int value ] -> West, value
    | ParseRegex @"L(\d+)" [ Int value ] -> Left, value
    | ParseRegex @"R(\d+)" [ Int value ] -> Right, value
    | ParseRegex @"F(\d+)" [ Int value ] -> Forward, value
    | _ -> failwith ("Unrecognised command input " + input)

let commands =
    File.ReadLines "./input.txt"
    |> Seq.map parseCommand
    |> Seq.toList

type Ship = {
    X: int;
    Y: int;
    Angle: int
    WaypointX: int
    WaypointY: int
}

let movePart1 (ship: Ship) (command: Action * int): Ship =
    match command with
    | North, value -> { ship with Y = ship.Y + value }
    | South, value -> { ship with Y = ship.Y - value }
    | East, value -> { ship with X = ship.X + value }
    | West, value -> { ship with X = ship.X - value }
    | Left, value -> { ship with Angle = (ship.Angle - value + 360) % 360 }
    | Right, value -> { ship with Angle = (ship.Angle + value + 360) % 360 }
    | Forward, value ->
        match ship.Angle with
        | 0 -> { ship with X = ship.X + value }
        | 90 -> { ship with Y = ship.Y - value }
        | 180 -> { ship with X = ship.X - value }
        | 270 -> { ship with Y = ship.Y + value }
        | _ -> failwith ("Unrecognised angle " + ship.Angle.ToString())

let initialShip = { X = 0; Y = 0; Angle = 0; WaypointX = 10; WaypointY = 1; }

let finalShipPart1 = Seq.fold movePart1 initialShip commands
printf "Part 1: Location (%d, %d). Manhattan distance: %d\r\n" finalShipPart1.X finalShipPart1.Y (Math.Abs finalShipPart1.X + Math.Abs finalShipPart1.Y)

let movePart2 (ship: Ship) (command: Action * int): Ship =
    match command with
    | North, value -> { ship with WaypointY = ship.WaypointY + value }
    | South, value -> { ship with WaypointY = ship.WaypointY - value }
    | East, value -> { ship with WaypointX = ship.WaypointX + value }
    | West, value -> { ship with WaypointX = ship.WaypointX - value }
    | Left, 90 | Right, 270 -> { ship with WaypointX = -ship.WaypointY; WaypointY = ship.WaypointX }
    | Left, 180 | Right, 180 -> { ship with WaypointX = -ship.WaypointX; WaypointY = -ship.WaypointY }
    | Left, 270 | Right, 90 -> { ship with WaypointX = ship.WaypointY; WaypointY = -ship.WaypointX }
    | Left, value | Right, value -> failwith ("Unrecognised angle: " + value.ToString())
    | Forward, value -> { ship with X = ship.X + ship.WaypointX * value; Y = ship.Y + ship.WaypointY * value }

let finalShipPart2 = Seq.fold movePart2 initialShip commands
printf "Part 2: Location (%d, %d). Manhattan distance: %d" finalShipPart1.X finalShipPart2.Y (Math.Abs finalShipPart2.X + Math.Abs finalShipPart2.Y)
