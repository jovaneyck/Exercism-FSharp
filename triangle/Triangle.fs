module Triangle
    type TriangleKind = 
        | Equilateral
        | Isosceles
        | Scalene

    let kind x y z = 
        let sides = x :: y :: z :: [] 

        let isNotATriangle =
                x + y <= z || y + z <= x || x + z <= y

        if sides |> Seq.exists (fun s -> s <= 0m) 
        then 
            raise <| new System.InvalidOperationException("All sides must be positive")
        elif isNotATriangle
        then
            raise <| new System.InvalidOperationException("Not a valid triangle.")
        else
            let nbDifferentLengths = 
                sides
                |> Seq.groupBy id 
                |> Seq.length
            match nbDifferentLengths with
            | 1 -> Equilateral
            | 2 -> Isosceles
            | _ -> Scalene