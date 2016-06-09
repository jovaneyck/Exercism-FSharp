module Atbash

open System.Text.RegularExpressions

let plain = ['a'..'z']
let cipher = plain |> List.rev
let encodeMap = Seq.zip plain cipher |> Map.ofSeq

let toString = (Seq.toArray >> System.String)
let toLower (s : string) = s.ToLower()
let stripNonAlphabet s = Regex.Replace(s, "[^(\w)]", "")
let sanitize = toLower >> stripNonAlphabet

let encodeLetter letter =
    match encodeMap |> Map.tryFind letter with
    | None -> letter
    | Some translation -> translation

let encode input : string = 
    input
    |> sanitize
    |> Seq.map encodeLetter
    |> Seq.chunkBySize 5
    |> Seq.map toString
    |> Seq.reduce (sprintf "%s %s")