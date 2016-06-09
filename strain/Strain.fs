module MySeq

let rec keep f xs = 
    seq {
        for x in xs do 
            if f x then yield x
    }

let discard f = keep (f >> not)