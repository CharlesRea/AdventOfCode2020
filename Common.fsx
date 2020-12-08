module Common

open System
open System.Text.RegularExpressions

let tryParseWith (tryParseFunc: string -> bool * _) = tryParseFunc >> function
    | true, v    -> Some v
    | false, _   -> None

let parseInt = tryParseWith System.Int32.TryParse
let parseDouble = tryParseWith System.Double.TryParse

let (|Int|_|)    = parseInt
let (|Double|_|) = parseDouble

let (|ParseRegex|_|) regex str =
   let m = Regex(regex).Match(str)
   if m.Success
   then Some (List.tail [ for x in m.Groups -> x.Value ])
   else None

let splitString (separator: string) (str: string): string[] =
    str.Split([| separator |] |> Seq.toArray, StringSplitOptions.None)

let printSequence (value: 'a seq) =
    value |> Seq.toList |> List.iter (printf "%A\r\n")

let replaceArrayElement (index: int) (newValue: 'a) (array: 'a array): 'a array =
    seq { for i in 0 .. array.Length - 1 -> if i = index then newValue else array.[i] } |> Seq.toArray
