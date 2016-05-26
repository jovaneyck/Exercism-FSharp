open System.Text.RegularExpressions

let vowels = ["a";"e"]

let(|StartsWithVowel|_|) input =
    match
        vowels 
        |> List.map (fun v -> sprintf "^(%s)(.*)" v)
        |> List.map (fun regex -> Regex.Match(input, regex))
        |> List.filter (fun possibleMatch -> possibleMatch.Success)
        |> List.map(fun m -> (m.Groups.[1].Value, m.Groups.[2].Value))
        |> List.tryHead
        with
        | None -> None
        | Some(vowel, remainder) -> Some <| StartsWithVowel(vowel, remainder)

match "estje" with
| StartsWithVowel(v, rest) -> printfn "Starts with %s - remainder %s" v rest
| _ -> printfn "No match"
