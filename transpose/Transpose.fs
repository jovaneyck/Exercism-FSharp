module Transpose

let splitLines (text : string) = text.Split([|'\n'|]) |> List.ofSeq
let trim (string : string) = string.TrimEnd()
let pad length (string : string) = string.PadRight(length)

let toMatrix rows =
    rows 
    |> List.map (fun row -> row |> Seq.map string |> List.ofSeq)
    |> array2D

let transposeMatrix m =
    [ for columnIndex in [0..Array2D.length2 m - 1] do 
        yield m.[*,columnIndex] 
    ]
    |> array2D

let toString m = 
    [for rowIndex in [0..Array2D.length1 m - 1] do
        let row = m.[rowIndex,*]
        yield row |> String.concat ""
    ]
    |> String.concat "\n"
    |> trim

let padRows rows = 
    let maxLength = 
        rows 
        |> Seq.map Seq.length
        |> Seq.max
    rows |> List.map (pad maxLength)

let transpose input = 
    input
    |> splitLines
    |> padRows
    |> toMatrix
    |> transposeMatrix
    |> toString