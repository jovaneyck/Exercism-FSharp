module Pangram

let alphabet = ['a'..'z']
let toLower (s : string) = s.ToLower()
let contains letter (s : string) = s.Contains(string letter)

let isPangram input = 
    let sanitizedInput = input |> toLower

    alphabet 
    |> Seq.forall (fun letter -> sanitizedInput |> contains letter)
