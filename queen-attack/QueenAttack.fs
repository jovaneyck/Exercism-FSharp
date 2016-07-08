module QueenAttack

open System

let sameRow (x1,_) (x2, _) = x1 = x2
let sameColumn (_,y1) (_, y2) = y1 = y2
let sameDiagonal (x1 : int, y1 : int) (x2, y2) = 
    Math.Abs(x2 - x1) =  Math.Abs(y2 - y1)

let canAttackValid p1 p2 = 
    [
        sameRow
        sameColumn
        sameDiagonal
    ]
    |> List.map (fun f -> f p1 p2)
    |> List.exists id

let canAttack f s = 
    if f = s then
        failwith "Queens cannot occupy the same space"
    else
        canAttackValid f s