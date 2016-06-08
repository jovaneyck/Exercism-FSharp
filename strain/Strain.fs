module Seq

let rec keep f xs = 
    [for x in xs do 
        if f x then yield x]

let discard f = keep (f >> not)