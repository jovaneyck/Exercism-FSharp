module RomanNumeral

open System

let toString = List.toArray >> String

//Let's do a two-phase algorithm as Sandi Metz shared in her latest newsletter!
//http://us3.campaign-archive2.com/?u=1090565ccff48ac602d0a84b4&id=24e2bac263&e=445a4e222b

let arabicToRoman = 
    [
        (1000, 'M')
        (500, 'D')
        (100, 'C')
        (50, 'L')
        (10, 'X')
        (5, 'V')
        (1, 'I')
    ]
    |> Map.ofSeq

let keys =
    arabicToRoman
    |> Map.toList
    |> List.sortByDescending (fun (key, _) -> key)
    |> List.map (fun (key,_) -> key)

let rec private expandedRoman acc arabic = 
    if arabic = 0 then acc
    else 
        let biggestArabicThatFits = keys |> List.find (fun k -> arabic >= k)
        let r = arabicToRoman |> Map.find biggestArabicThatFits
        expandedRoman (acc @ [r]) (arabic - biggestArabicThatFits)

let private simplify expanded = 
    let rec simple acc expanded = 
        match expanded with
        | [] -> acc
        | 'D'::'C'::'C'::'C'::'C'::rest -> simple (acc @ ['C';'M']) rest
        | 'C'::'C'::'C'::'C'::rest -> simple (acc @ ['C';'D']) rest
        | 'L'::'X'::'X'::'X'::'X'::rest -> simple (acc @ ['X';'C']) rest
        | 'X'::'X'::'X'::'X'::rest -> simple (acc @ ['X';'L']) rest
        | 'V'::'I'::'I'::'I'::'I'::rest -> simple (acc @ ['I';'X']) rest
        | 'I'::'I'::'I'::'I'::rest -> simple (acc @ ['I';'V']) rest
        | h::t -> simple (acc @ [h]) t

    simple [] expanded

let toRoman arabic =    
    arabic
    |> expandedRoman []
    |> simplify
    |> toString