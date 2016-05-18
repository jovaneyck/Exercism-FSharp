module SumOfMultiples

let sumOfMultiples numbers max = 
    [1..max-1]
    |> Seq.filter 
        (fun candidate -> 
            numbers 
            |> Seq.exists (fun nb -> candidate % nb = 0))
    |> Seq.sum