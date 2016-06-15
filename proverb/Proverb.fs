module Proverb

let words = 
    ["nail"; "shoe"; "horse"; "rider"; "message"; "battle"; "kingdom"]

let proverbLines = 
    let seventh = 
        "And all for the want of a horseshoe nail."
    let verses =
        words
        |> Seq.pairwise
        |> Seq.map (fun (f,s) -> 
            sprintf "For want of a %s the %s was lost." f s)

    Seq.append verses [seventh]

let proverb = proverbLines |> (String.concat "\n")

let line number =
    proverbLines 
    |> Seq.indexed
    |> Seq.find (fun (i, _) -> i + 1 = number)
    |> snd