module Atbash

open System
open System.Text.RegularExpressions

let letters = ['a'..'z']
let numbers = ['0'..'9']
let encodeMap =
    List.append
        (List.zip letters (letters |> List.rev))
        (List.zip numbers numbers)
    |> Map.ofList
let encodeOne char = encodeMap |> Map.find char

let toLower (s : string) = s.ToLower()
let stripNonAlphabet s = Regex.Replace(s, "[^\w]", "")
let sanitize = toLower >> stripNonAlphabet

let encode input : string = 
    input
    |> sanitize
    |> Seq.map encodeOne
    |> Seq.chunkBySize 5
    |> Seq.map String
    |> String.concat " "