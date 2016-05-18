module DifferenceOfSquares

let squared n = n * n

let squareOfSums n = 
    [1..n]
    |> Seq.sum
    |> squared

let sumOfSquares n = 
    [1..n]
    |> Seq.map squared
    |> Seq.sum

let difference n = squareOfSums n - sumOfSquares n