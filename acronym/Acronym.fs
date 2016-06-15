module Acronym

open System
open System.Text.RegularExpressions

let split s = 
    Regex.Split(s, "[^\w]")
    |> List.ofArray
    |> List.filter (fun s -> not (String.IsNullOrWhiteSpace(s)))
    |> List.map List.ofSeq

let upperCaseFirstLetter word = 
    match word with
    | [] -> []
    | l :: ls -> Char.ToUpper(l) :: ls

let keepRelevantUppercased (word : char List) =
    let toUpper word = 
        let rec up w acc = 
            match w with
            | [] -> acc
            | l :: ls -> up ls ((l |> Char.ToUpper) :: acc)
        up word [] |> List.rev
    let allUppercase w = 
        w = (w |> toUpper)
    let onlyFirstLetter w =
        w
        |> List.head
        |> List.singleton

    if allUppercase word then
        onlyFirstLetter word
    else
        word

let keepUpperCaseLetters word = 
    word 
    |> List.filter Char.IsUpper

let toString = Array.ofList >> System.String

let acronym (text : string) = 
    text 
    |> split
    |> List.map upperCaseFirstLetter
    |> List.map keepRelevantUppercased
    |> List.map keepUpperCaseLetters
    |> List.map toString
    |> String.concat ""