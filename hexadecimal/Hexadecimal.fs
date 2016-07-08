module Hexadecimal

let baseValue = 16
let digitsToDecimalValue = 
    [
        ('0', 0)
        ('1', 1)
        ('2', 2)
        ('3', 3)
        ('4', 4)
        ('5', 5)
        ('6', 6)
        ('7', 7)
        ('8', 8)
        ('9',9)
        ('a', 10)
        ('b', 11)
        ('c', 12)
        ('d', 13)
        ('e', 14)
        ('f', 15)
    ] |> Map.ofSeq

let add f s = 
    match (f,s) with
    | None, _ -> None
    | _, None -> None
    | Some fst, Some snd -> Some (baseValue * fst + snd)

let toDecimal hex = 
    hex
    |> Seq.map (fun digit -> digitsToDecimalValue |> Map.tryFind digit)
    |> Seq.reduce add
    |> Option.fold (+) 0