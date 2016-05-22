module Triangle
    
    open System

    type TriangleKind = 
        | Equilateral
        | Isosceles
        | Scalene

    let kind x y z = 
        let sides = x :: y :: z :: [] 
        let isATriangle = x + y > z && y + z > x && x + z > y

        if sides |> Seq.exists (fun s -> s <= 0m) 
        then 
            raise <| new InvalidOperationException("All sides must be positive")
        elif not isATriangle
        then
            raise <| new InvalidOperationException("Not a valid triangle.")
        else
            let nbDifferentLengths = 
                Set sides |> Set.count
            match nbDifferentLengths with
            | 1 -> Equilateral
            | 2 -> Isosceles
            | _ -> Scalene