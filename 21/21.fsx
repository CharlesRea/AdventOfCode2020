#load "../Common.fsx"

open System
open System.IO
open Common
open FSharp.Collections

type Ingredient = Ingredient of string
type Allergen = Allergen of string

type Food = {
    ingredients: Ingredient[]
    allergens: Allergen[]
}

let parseFood (input: string) =
    match input with
    | ParseRegex @"(.*) \(contains (.*)\)" [ ingredients; allergens ] ->
        { ingredients = splitString " " ingredients |> Array.map Ingredient
          allergens = splitString ", " allergens |> Array.map Allergen }
    | input ->
        { ingredients = splitString " " input |> Array.map Ingredient
          allergens = Array.empty }

let foods =
    File.ReadLines "./input.txt"
    |> Seq.map parseFood
    |> Seq.toList

type State = {
    solvedAllergens: Map<Allergen, Ingredient>
    solvedIngredients: Map<Ingredient, Allergen>
    unsolvedIngredients: Ingredient Set
    unsolvedAllergens: Allergen Set
}

let identifyAllergens (foods: Food list): Map<Ingredient, Allergen> =
    let foodsByAllergen =
        foods
        |> Seq.map (fun food -> food.allergens |> Seq.map (fun allergen -> (allergen, food)))
        |> Seq.concat
        |> toLookup

    let rec runStep (state: State): State option =
        let possibleIngredients (allergen: Allergen): Ingredient Set =
            foodsByAllergen
            |> Map.find allergen
            |> Seq.map (fun food -> Set.ofArray food.ingredients)
            |> Set.intersectMany
            |> Set.intersect state.unsolvedIngredients

        match (Set.isEmpty state.unsolvedAllergens) with
        | true -> Some state
        | _ ->
            let allergenGuesses =
                state.unsolvedAllergens
                |> Seq.map (fun allergen -> possibleIngredients allergen |> Seq.map (fun ingredient -> (allergen, ingredient)))
                |> Seq.concat

            allergenGuesses
            |> Seq.map (fun (allergen, ingredient) ->
                                  runStep { state with
                                                  solvedAllergens = state.solvedAllergens |> Map.add allergen ingredient
                                                  solvedIngredients = state.solvedIngredients |> Map.add ingredient allergen
                                                  unsolvedAllergens = state.unsolvedAllergens |> Set.remove allergen
                                                  unsolvedIngredients = state.unsolvedIngredients |> Set.remove ingredient })
            |> Seq.filter Option.isSome
            |> Seq.map Option.get
            |> Seq.tryHead

    let state =
        { solvedAllergens = Map.empty
          solvedIngredients = Map.empty
          unsolvedAllergens = foods |> Seq.map (fun food -> food.allergens |> Set.ofArray) |> Set.unionMany
          unsolvedIngredients = foods |> Seq.map (fun food -> food.ingredients |> Set.ofArray) |> Set.unionMany }

    let finalState = runStep state

    match finalState with
    | Some finalState -> finalState.solvedIngredients
    | None -> failwith "No valid solution found"


let allergens = identifyAllergens foods
printf "Found allergens:\n"
printSequence allergens
printf "\n\n"

let allIngredients = foods |> Seq.map (fun food -> food.ingredients |> Set.ofArray) |> Set.unionMany

let ingredientsWithoutAllergens =
    allIngredients
    |> Set.filter (fun ingredient -> not (Map.containsKey ingredient allergens))

let allIngredientOccurrences = foods |> Seq.map (fun food -> food.ingredients) |> Seq.concat

let occurrencesOfIngredientsWithoutAllergens =
    ingredientsWithoutAllergens
    |> Seq.map (fun ingredient -> allIngredientOccurrences |> Seq.filter ((=) ingredient) |> Seq.length)
    |> Seq.sum

printf "Part 1: %d\n\n" occurrencesOfIngredientsWithoutAllergens

let dangerousIngredients =
    allergens
    |> Map.toSeq
    |> Seq.sortBy snd
    |> Seq.map fst
    |> Seq.map (fun (Ingredient ingredient) -> ingredient)
    |> String.concat ","

printf "Part 2: %s" dangerousIngredients