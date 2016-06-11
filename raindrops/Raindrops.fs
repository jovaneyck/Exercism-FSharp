module Raindrops

let convert number =
    let specialCases = [(3, "Pling"); (5, "Plang"); (7, "Plong")]
    let translations = 
        [for (d,w) in specialCases do 
            if number % d = 0 then yield w]

    match translations with
    | [] -> string number
    | t  -> t |> String.concat ""
    