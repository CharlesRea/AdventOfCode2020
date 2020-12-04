module Common

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
