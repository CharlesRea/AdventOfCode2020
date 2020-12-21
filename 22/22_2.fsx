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

type Player = Player1 | Player2

type Game = {
    winner: Player
    winningDeck: Deck
}

let roundAlreadyPlayed (player1: Deck) (player2: Deck) (previousHands: (Deck * Deck) list): bool =
    let equals (deck1: Deck) (deck2: Deck): bool =
        List.length deck1 = List.length deck2 && (List.compareWith Operators.compare) deck1 deck2 = 0

    previousHands
    |> Seq.exists (fun (prevPlayer1, prevPlayer2) -> equals player1 prevPlayer1 && equals player2 prevPlayer2)

let rec playGame (player1: Deck) (player2: Deck) (previousHands: (Deck * Deck) list): Game =
    match roundAlreadyPlayed player1 player2 previousHands with
    | true -> { winner = Player1; winningDeck = player1 }
    | false ->
        match player1, player2 with
        | card1 :: player1Deck, card2 :: player2Deck ->
            let nextHistory = ((player1, player2) :: previousHands)
            match card1 <= List.length player1Deck && card2 <= List.length player2Deck with
            | true ->
                let subGame = playGame (player1Deck |> List.take card1) (player2Deck |> List.take card2) List.empty
                match subGame.winner with
                | Player1 -> playGame (player1Deck @ [ card1; card2; ]) player2Deck nextHistory
                | Player2 -> playGame player1Deck (player2Deck @ [ card2; card1; ]) nextHistory
            | false ->
                match card1 > card2 with
                | true -> playGame (player1Deck @ [ card1; card2; ]) player2Deck nextHistory
                | false -> playGame player1Deck (player2Deck @ [ card2; card1; ]) nextHistory
        | [], player2 -> { winner = Player2; winningDeck = player2 }
        | player1, [] -> { winner = Player1; winningDeck = player1 }

let finalDeck = playGame player1 player2 List.empty

printf "Final deck: \n"
printSequence finalDeck.winningDeck
printf "\n"

let deckScore =
    finalDeck.winningDeck
    |> List.rev
    |> List.indexed
    |> List.map (fun (i, x) -> (i + 1) * x)
    |> List.sum

printf "Deck score: %d" deckScore