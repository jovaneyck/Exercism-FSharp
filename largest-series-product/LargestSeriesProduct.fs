module LargestSeriesProduct

let largestProduct (digits : string) length = 
    if digits = "" && length > 0 then
        failwith "Cannot slice an empty string"
    elif length <= 0 then
        1
    else
        digits
        |> Seq.map (string >> System.Int32.Parse)
        |> Seq.windowed length
        |> Seq.map (Seq.fold (*) 1)
        |> Seq.max