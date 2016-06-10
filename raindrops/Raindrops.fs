module Raindrops

let convert number =
    let specialCases = [(3, "Pling"); (5, "Plang"); (7, "Plong")]
    let translations = 
        [for (d,w) in specialCases do 
            if number % d = 0 then yield w]

    if translations |> List.isEmpty then
        string number
    else
        translations |> String.concat ""
    