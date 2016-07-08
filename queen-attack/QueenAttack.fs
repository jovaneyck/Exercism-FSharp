module QueenAttack

let canAttack ((x1, y1) as f) ((x2, y2) as s) = 
    if f = s then
        invalidOp "Queens cannot occupy the same space"
    else
        x1 = x2 
        || y1 = y2 
        || (x2 - x1 |> abs) = (y2 - y1 |> abs)