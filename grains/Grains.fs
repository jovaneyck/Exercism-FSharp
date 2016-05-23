module Grains

let square n =
    let rec sq acc n =
        if n = 1 then acc
        else sq (2I * acc) (n - 1)
    sq 1I n

let total = 
    [1..64]
    |> Seq.map square
    |> Seq.sum