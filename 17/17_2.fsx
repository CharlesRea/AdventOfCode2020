#load "../Common.fsx"

open System.IO
open Common

type Cube = int * int * int * int

let initialCubes: Cube Set =
    File.ReadLines "./input.txt"
    |> Seq.mapi (fun y row -> row |> Seq.mapi (fun x cell -> (cell, x, y)))
    |> Seq.reduce Seq.append
    |> Seq.filter (fun (cell, _, _) -> cell = '#')
    |> Seq.map (fun (cell, x, y) -> (x, y, 0, 0))
    |> Set.ofSeq

let neighbours ((x, y, z, w): Cube): Cube Set =
    allQuadruples (seq { -1..1 }) (seq { -1..1 }) (seq { -1..1 }) (seq { -1..1 })
    |> Seq.filter (fun vector -> vector <> (0, 0, 0, 0))
    |> Seq.map (fun (x2, y2, z2, w2) -> (x + x2, y + y2, z + z2, w + w2))
    |> Set.ofSeq

let isNextCubeActive (activeCubes: Cube Set) (cube: Cube): bool =
    let isActive = activeCubes |> Set.contains cube
    let activeNeighbours = neighbours cube |> Set.intersect activeCubes |> Set.count

    match (isActive, activeNeighbours) with
    | true, (2 | 3) -> true
    | false, 3 -> true
    | _ -> false

let nextCubes (cubes: Cube Set): Cube Set =
    let range (projection: Cube -> int): int seq =
        seq { (cubes |> Seq.map projection |> Seq.min) - 1 .. (cubes |> Seq.map projection |> Seq.max) + 1 }

    let coordsToCheck =
        allQuadruples
            (range (fun (x, _, _, _) -> x))
            (range (fun (_, y, _, _) -> y))
            (range (fun (_, _, z, _) -> z))
            (range (fun (_, _, _, w) -> w))

    coordsToCheck |> Seq.filter (isNextCubeActive cubes) |> Set.ofSeq

let finalCubes = seq { 0..5 } |> Seq.fold (fun cubes _ -> nextCubes cubes) initialCubes

printf "%d" (Set.count finalCubes)