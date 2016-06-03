module RomanNumeral

open System

let private toString = List.toArray >> String

//Let's do a two-pass algorithm like the one Sandi Metz shared in her latest newsletter!
//http://us3.campaign-archive2.com/?u=1090565ccff48ac602d0a84b4&id=24e2bac263&e=445a4e222b

let private arabicToRoman = 
    [
        (1,     'I')
        (5,     'V')
        (10,    'X')
        (50,    'L')
        (100,   'C')
        (500,   'D')
        (1000,  'M')
    ]
    |> Map.ofSeq

let private keys map =
    map
    |> Map.toList
    |> List.map (fun (key,_) -> key)

let rec private expandedRoman acc arabic = 
    if arabic = 0 then acc
    else 
        let biggestArabicThatFits = 
                arabicToRoman 
                |> keys 
                |> List.sortDescending 
                |> List.find (fun k -> arabic >= k)
        let r = 
                arabicToRoman 
                |> Map.find biggestArabicThatFits
        expandedRoman (acc @ [r]) (arabic - biggestArabicThatFits)

let private simplify expanded = 
    let rec simpler acc expanded = 
        match expanded with
        | [] -> acc
        //Not really happy with the noise here
        | 'D'::'C'::'C'::'C'::'C'::rest -> simpler (acc @ ['C';'M']) rest
        | 'C'::'C'::'C'::'C'::rest -> simpler (acc @ ['C';'D']) rest
        | 'L'::'X'::'X'::'X'::'X'::rest -> simpler (acc @ ['X';'C']) rest
        | 'X'::'X'::'X'::'X'::rest -> simpler (acc @ ['X';'L']) rest
        | 'V'::'I'::'I'::'I'::'I'::rest -> simpler (acc @ ['I';'X']) rest
        | 'I'::'I'::'I'::'I'::rest -> simpler (acc @ ['I';'V']) rest
        | h::t -> simpler (acc @ [h]) t

    simpler [] expanded

let toRoman arabic =    
    arabic
    |> expandedRoman []
    |> simplify
    |> toString