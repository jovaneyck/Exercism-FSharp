module Proverb

let words = 
    ["nail"; "shoe"; "horse"; "rider"; "message"; "battle"; "kingdom"]

let line number =
    match number with
    | 7 ->
        "And all for the want of a horseshoe nail."
    | n ->
        words
        |> Seq.pairwise
        |> Seq.item (n - 1)
        |> (fun (f,s) -> sprintf "For want of a %s the %s was lost." f s)

let proverb = 
    [1..7] 
    |> Seq.map line
    |> String.concat "\n"

