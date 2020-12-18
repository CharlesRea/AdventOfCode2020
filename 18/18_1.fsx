#load "../Common.fsx"

open System.IO
open Common
open System.Text.RegularExpressions

let (|Operator|_|) (str): (int64 -> int64 -> int64) option =
   match str with
   | "+" -> Some (+)
   | "*" -> Some (*)
   | _ -> failwith $"Invalid operator {str}"

let rec evaluate (expression: string): int64 =
    match expression.Trim() with
    | ParseRegex @"^(\d+)$" [ Int64 value ] -> value
    | ParseRegex @"^(.*)(\((?>\((?<c>)|[^()]+|\)(?<-c>))*(?(c)(?!))\))(.*)$" [ lhs; bracket; rhs; _ ] -> evaluate $"{lhs}{evaluate (bracket.Trim('(', ')'))}{rhs}"
    | ParseRegex @"^(.+) (\+|\*) (\d+)$" [ lhs; Operator operator; Int64 rhs ] -> operator (evaluate lhs) rhs
    | _ -> failwith $"Invalid expression: {expression}"

let runTests () =
    let testCases = [
        ("1 + 2 * 3 + 4 * 5 + 6", 71L);
        ("2 * 3 + (4 * 5)", 26L);
        ("5 + (8 * 3 + 9 + 3 * 4 * 3)", 437L);
        ("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))", 12240L);
        ("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2", 13632L);
    ]

    for (testCase, expectedValue) in testCases do
        printf $"Testing {testCase}\n"
        let value = evaluate testCase
        if value = expectedValue then
            printf "Answer was correct\n"
        else
            printf $"Incorrect answer. Expected {expectedValue}, got {value}\n"
        printf "\n"

runTests()

let part1 () =
    File.ReadLines "./input.txt"
    |> Seq.map evaluate
    |> Seq.sum

printf "Part 1: %d" (part1())