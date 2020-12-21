open System
#load "../Common.fsx"

open System.IO
open Common
open FSharp.Collections

type Match =
    | Tile of int
    | Edge


type Tile =
    { id: int
      data: char[,]
      top: string
      right: string
      bottom: string
      left: string }

let createTile (data: char[,]) (id: int): Tile =
    { id = id
      data = data
      top = data.[0, *] |> joinCharsToString
      right = data.[*, Array2D.length2 data - 1] |> joinCharsToString
      bottom = data.[Array2D.length2 data - 1, *] |> joinCharsToString
      left = data.[*, 0] |> joinCharsToString
    }

let stringsToArray2d (strings: string array) =
    Array2D.init strings.Length strings.Length (fun row column -> (strings.[column]).[row])

let parseTile (input: string) =
    let rows = input |> splitString Environment.NewLine
    let data = stringsToArray2d rows.[1..]

    let id =
        match Seq.head rows with
        | ParseRegex "Tile (\d+):" [ Int id ] -> id
        | _ -> failwith "Invalid tile ID input"

    createTile data id

let tiles =
    File.ReadAllText "./input.txt"
    |> splitString (Environment.NewLine + Environment.NewLine)
    |> Array.map parseTile
    |> Array.toList

let imageTileLength = tiles.Length |> double |> sqrt |> int

type BorderCondition =
    | Edge
    | Border of string
    | TileNotYetPlaced

let rotate90 (data: char[,]): char[,] =
    let height, width = Array2D.length1 data, Array2D.length2 data
    Array2D.init width height (fun row column -> Array2D.get data (height - column - 1) row)

let reflectX (data: char[,]): char[,] =
    let height, width = Array2D.length1 data, Array2D.length2 data
    Array2D.init width height (fun row column -> Array2D.get data row (width - column - 1))

let transformations = [
    id
    rotate90
    rotate90 >> rotate90
    rotate90 >> rotate90 >> rotate90
    reflectX
    reflectX >> rotate90
    reflectX >> rotate90 >> rotate90
    reflectX >> rotate90 >> rotate90 >> rotate90
]

let meetsCondition (condition: BorderCondition) (otherTiles: Tile seq) (border: string) =
    let matchingBorders = otherTiles |> Seq.map (fun tile -> [ tile.top; tile.right; tile.bottom; tile.left; ]) |> Seq.concat |> Seq.filter ((=) border)

    match condition with
    | Border expectedBorder -> border = expectedBorder
    | Edge -> matchingBorders |> Seq.isEmpty
    | TileNotYetPlaced -> true

let findTiles (topBorder: BorderCondition) (leftBorder: BorderCondition) (tiles: Tile list): Tile list =
    let matches =
        tiles
        |> Seq.map (fun tile -> tile, tiles |> Seq.except (Seq.singleton tile))
        |> Seq.map (fun (tile, otherTiles) -> transformations |> Seq.map (fun transformation -> (createTile (transformation tile.data) tile.id, otherTiles)))
        |> Seq.concat
        |> Seq.filter (fun (tile, otherTiles) -> tile.top |> meetsCondition topBorder otherTiles)
        |> Seq.filter (fun (tile, otherTiles) -> tile.left |> meetsCondition leftBorder otherTiles)
        |> Seq.map fst
        |> Seq.toList

    matches

let printTiles (tiles: Map<int * int, Tile>) =
    for y in seq { 0..imageTileLength - 1 } do
        for x in seq { 0..imageTileLength - 1 } do
            let tile = tiles |> Map.tryFind (x, y) |> Option.map (fun tile -> tile.id |> string) |> Option.defaultValue "----"
            printf "%s " tile
        printf "\n"

let combineTiles (tiles: Tile list): Map<(int * int), Tile> =
    let rec placeTile (placedTiles: Map<(int * int), Tile>) (unplacedTiles: Tile list) ((x, y): int * int): Map<(int * int), Tile> option =
        match unplacedTiles, x, y with
        | [], _ , _ -> Some placedTiles
        | _ ->
            let topBorder =
                match y with
                | 0 -> Edge
                | _ -> Border (Map.find (x, y - 1) placedTiles).bottom

            let leftBorder =
                match x with
                | 0 -> Edge
                | _ -> Border (Map.find (x - 1, y) placedTiles).right

            let nextCoord =
                match (x, y) with
                | (x, y) when x = imageTileLength - 1 -> (0, y + 1)
                | _ -> (x + 1, y)

            let tiles = findTiles topBorder leftBorder unplacedTiles

            match tiles with
            | [] ->  None
            | tiles ->
                tiles
                |> Seq.map (fun tile -> placeTile (placedTiles |> Map.add (x, y) tile) (unplacedTiles |> List.filter (fun t -> t.id <> tile.id)) nextCoord)
                |> Seq.filter Option.isSome
                |> Seq.map Option.get
                |> Seq.tryHead

    let placedTiles = placeTile Map.empty tiles (0, 0)

    match placedTiles with
    | Some placedTiles -> placedTiles
    | None -> failwith "No valid tile ordering found"

let cornerProduct (tiles: Map<int * int, Tile>) =
    [tiles.[(0,0)]; tiles.[(0, imageTileLength - 1)]; tiles.[(imageTileLength - 1, 0)]; tiles.[imageTileLength - 1, imageTileLength - 1]]
    |> List.map (fun tile -> int64 tile.id)
    |> List.reduce (*)

let placedTiles = combineTiles tiles

printf "Placed tiles:\n"
printTiles placedTiles
printf "Part 1: %d\n\n" (cornerProduct placedTiles)

let combineTileData (tiles: Map<int * int, Tile>) =
    let tileSize = tiles |> Map.find (0,0) |> (fun tile -> tile.data |> Array2D.length1)
    let size = imageTileLength * (tileSize - 2)

    let getCharacter x y =
        let tileCoords = y / (tileSize - 2), x / (tileSize - 2)
        let tile = tiles.[tileCoords]
        tile.data.[x % (tileSize - 2) + 1, y % (tileSize - 2) + 1]

    Array2D.init size size getCharacter

let tileData = combineTileData placedTiles

printf "Tile data: \n"
for y in seq { 0..Array2D.length2 tileData - 1 } do
    for x in seq { 0..Array2D.length1 tileData - 1 } do
        printf "%c" tileData.[y, x]
    printf "\n"
printf "\n"


let seaMonsterData =
    ["                  # "
     "#    ##    ##    ###"
     " #  #  #  #  #  #   "];

let seaMonsterCoordinates =
    seaMonsterData
    |> Seq.map (Seq.indexed)
    |> Seq.indexed
    |> Seq.map (fun (y, row) -> row |> Seq.map (fun (x, value) -> (x, y, value)))
    |> Seq.concat
    |> Seq.filter (fun (_, _, char) -> char = '#')
    |> Seq.map (fun (x, y, char) -> (x, y))
    |> Set.ofSeq

let isSeaMonsterAt (data: char[,]) ((x0, y0): int * int): bool =
    seaMonsterCoordinates
    |> Seq.map (fun (x1, y1) -> (x0 + x1, y0 + y1))
    |> Seq.filter (fun (x, y) -> data.[x, y] <> '#')
    |> Seq.isEmpty

let waterRoughness (data: char[,]) =
    let seaMonsterHeight = seaMonsterData.Length
    let seaMonsterWidth = seaMonsterData.[0].Length
    let dataWidth = Array2D.length1 data
    let dataHeight = Array2D.length2 data
    let seaMonsterOccurrences =
        Seq.allPairs (seq { 0..dataWidth - seaMonsterWidth }) (seq { 0..dataHeight - seaMonsterHeight })
        |> Seq.filter (isSeaMonsterAt data)

    let seaMonsterCharacters =
        seaMonsterOccurrences
        |> Seq.map (fun (x, y) -> seaMonsterCoordinates |> Seq.map (fun (x1, y1) -> (x + x1, y + y1)))
        |> Seq.concat
        |> Set.ofSeq

    data
        |> Array2D.mapi (fun x y value -> (x, y, value))
        |> Seq.cast<int*int*char>
        |> Seq.filter (fun (x, y, value) -> value = '#')
        |> Seq.filter (fun (x, y, value) -> not (Set.contains (x, y) seaMonsterCharacters))
        |> Seq.length

let minWaterRoughness =
    transformations
    |> Seq.map (fun transformation -> transformation tileData)
    |> Seq.map waterRoughness
    |> Seq.min

printf "Part 2: %d" minWaterRoughness