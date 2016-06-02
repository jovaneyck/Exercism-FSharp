module Phrase

open System.Text.RegularExpressions

let chunk phrase = Regex.Replace(phrase, "[^(\w|\')]", " ")

let split (s : string) = 
    s.Split([|" "|], System.StringSplitOptions.RemoveEmptyEntries)

let erase regex phrase = 
    Regex.Replace(phrase, regex, "")

let toLower (s : string) = s.ToLower()

let sanitize word = 
    let charactersNotInAlphabet = "[^(\w|\')]"
    let nonLettersAtStart = "^[^(\w)]*"
    let nonLettersAtEnd = "[^(\w)]*$"

    word
    |> erase charactersNotInAlphabet
    |> erase nonLettersAtStart
    |> erase nonLettersAtEnd
    |> toLower

let containsALetter w = Regex.Match(w, "\w+").Success

let wordCount phrase =
    phrase
    |> chunk
    |> split
    |> Seq.map (sanitize)
    |> Seq.filter containsALetter
    |> Seq.countBy id
    |> Map.ofSeq