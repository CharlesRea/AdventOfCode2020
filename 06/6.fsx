#load "../Common.fsx"

open System.IO
open Common

let sumWhereAnyoneAnswered =
    File.ReadAllText("./input.txt")
    |> splitString "\r\n\r\n"
    |> Seq.sumBy (fun group -> (group.Replace("\r\n", "") |> Seq.distinct |> Seq.length))

printf "Part 1: %A\r\n" sumWhereAnyoneAnswered

let countAnsweredByAll (group: string) =
    group
    |> splitString "\r\n"
    |> Seq.map (Seq.toArray >> Set.ofArray)
    |> Set.intersectMany
    |> Set.count

let sumWhereEveryoneAnswered =
    File.ReadAllText("./input.txt")
    |> splitString "\r\n\r\n"
    |> Seq.sumBy (countAnsweredByAll)

printf "Part 2: %A\r\n" sumWhereEveryoneAnswered
