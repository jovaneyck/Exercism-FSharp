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

let apply f x y = 
    match x, y with
    | None, _ -> None
    | _, None -> None
    | Some sx, Some sy -> Some (f sx sy)

let add f s = baseValue * f + s

let getOrElse defaultValue o =
    match o with
    | Some v -> v
    | None -> defaultValue

let toDecimal hex = 
    hex
    |> Seq.map (fun digit -> Map.tryFind digit digitsToDecimalValue)
    |> Seq.reduce (apply add)
    |> getOrElse 0