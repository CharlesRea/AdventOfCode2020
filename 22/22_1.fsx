#load "../Common.fsx"

open System
open System.IO
open Common
open FSharp.Collections

type Deck = int list

let decks =
    File.ReadAllText "./input.txt"
    |> splitString (Environment.NewLine + Environment.NewLine)
    |> Array.map (splitString Environment.NewLine >> Array.skip 1 >> Array.map int >> List.ofArray)

let player1, player2 = decks.[0], decks.[1]

let rec playGame (player1: Deck) (player2: Deck): Deck =
    match player1, player2 with
    | card1 :: player1Deck, card2 :: player2Deck ->
        match card1 > card2 with
        | true -> playGame (player1Deck @ [ card1; card2; ]) player2Deck
        | false -> playGame player1Deck (player2Deck @ [ card2; card1; ])
    | [], player2 -> player2
    | player1, [] -> player1

let finalDeck = playGame player1 player2

let deckScore =
    finalDeck
    |> List.rev
    |> List.indexed
    |> List.map (fun (i, x) -> (i + 1) * x)
    |> List.sum

printf "%d" deckScore