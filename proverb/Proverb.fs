module Proverb

let words = 
    ["nail"; "shoe"; "horse"; "rider"; "message"; "battle"; "kingdom"]

let line number =
    if number = 7 then
        "And all for the want of a horseshoe nail."
    else
        match words.[number - 1 .. number] with
        | [f; s] ->
            sprintf "For want of a %s the %s was lost." f s
        | _ -> 
            failwith <| sprintf "don't know line %d" number

let proverb = 
    [1..7]
    |> List.map line
    |> String.concat "\n"