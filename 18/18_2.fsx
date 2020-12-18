#load "../Common.fsx"

open System.IO
open Common
open System.Text.RegularExpressions

let rec evaluate (expression: string): int64 =
    match expression.Trim() with
    | ParseRegex @"^(\d+)$" [ Int64 value ] -> value
    | ParseRegex @"^(.*)(\((?>\((?<c>)|[^()]+|\)(?<-c>))*(?(c)(?!))\))(.*)$" [ lhs; bracket; rhs; _ ] -> evaluate $"{lhs}{evaluate (bracket.Trim('(', ')'))}{rhs}"
    | ParseRegex @"^(.+) \* (.+)$" [ lhs; rhs ] -> (evaluate lhs) * (evaluate rhs)
    | ParseRegex @"^(.+) \+ (.+)$" [ lhs; rhs ] -> (evaluate lhs) + (evaluate rhs)
    | _ -> failwith $"Invalid expression: {expression}"

let runTests () =
    let testCases = [
        ("1 + (2 * 3) + (4 * (5 + 6))", 51L);
        ("2 * 3 + (4 * 5)", 46L);
        ("5 + (8 * 3 + 9 + 3 * 4 * 3)", 1445L);
        ("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))", 669060L);
        ("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2", 23340L);
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

let part2 () =
    File.ReadLines "./input.txt"
    |> Seq.map evaluate
    |> Seq.sum

printf "Part 2: %d" (part2())