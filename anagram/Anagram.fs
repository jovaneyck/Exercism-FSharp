module Anagram

let toLower (word : string) = word.ToLower()

let canonicalRepresentation word = 
    word 
    |> toLower
    |> List.ofSeq 
    |> List.sort

let areAnagrams a b = 
    (toLower a <> toLower b) 
    && canonicalRepresentation a = canonicalRepresentation b

let anagrams dictionary word = 
    dictionary
    |> Set.ofSeq
    |> Seq.filter (areAnagrams word)