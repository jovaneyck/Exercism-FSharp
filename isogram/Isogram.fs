module Isogram

open System.Text.RegularExpressions

let relevantCharacters = 
    ['a'..'z'] @ ['A'..'Z'] @ ['à'..'Ȁ']
let isRelevantCharacter ch =
    relevantCharacters 
    |> List.contains ch
let onlyRelevantCharacters = 
    Seq.filter isRelevantCharacter

let toLower (str : string) = str.ToLower()

let isogram (sentence : string) = 
    let relevantCharacters =
        sentence
        |> toLower
        |> onlyRelevantCharacters
        |> List.ofSeq
    let nbTotalLetters = 
        relevantCharacters
        |> List.length
    let nbDifferentLetters =
        relevantCharacters
        |> List.groupBy id
        |> List.length
    nbTotalLetters = nbDifferentLetters
