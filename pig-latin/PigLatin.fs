module PigLatin

open System
open System.Text.RegularExpressions

let toString = List.toArray >> String
let split (s: string) = s.Split([|' '|])
let join = Seq.reduce (fun l r -> l + " " + r)

let vowelish = 
    ["a";"e";"i";"o";"u";"yt";"xr"]
let consonantish = 
    //Don't like the inline regex 
    // + fact that order is important for overlapping patterns :(
    ["qu"; "ch"; ".+qu"; "thr"; "th"; "sch"]

let startsWithAnyOf chunks word =
    chunks 
    |> List.map (fun v -> sprintf "^(%s)(.*)" v)
    |> List.map (fun regex -> Regex.Match(word, regex))
    |> List.filter (fun possibleMatch -> possibleMatch.Success)
    |> List.map(fun m -> (m.Groups.[1].Value, m.Groups.[2].Value))
    |> List.tryHead

let (|StartsWithAVowel|_|) word = 
    match word |> startsWithAnyOf vowelish with
    | Some(vowel, remainder) -> Some StartsWithAVowel
    | None -> None

let (|StartsWithConsonant|_|) word =
    match word |> startsWithAnyOf consonantish with
    | Some(consonant, remainder) -> 
        Some <| StartsWithConsonant(consonant, remainder)
    | None ->
        match word |> List.ofSeq with
        | [] -> None
        | h :: t -> Some <| StartsWithConsonant(string h,t |> toString)

let translateWord word =
    match word with
    | StartsWithAVowel -> word + "ay"
    | StartsWithConsonant(consonant, remainder) -> remainder + consonant + "ay"
    | _ -> word

let translate sentence = 
    sentence
    |> split
    |> Seq.map translateWord
    |> join