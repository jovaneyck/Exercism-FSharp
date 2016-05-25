module Hamming

let compute strand1 strand2 = 
    strand1
    |> Seq.zip strand2
    |> Seq.filter (fun (s1, s2) -> s1 <> s2)
    |> Seq.length
