module PascalsTriangle

let sum left right = 
    match (left, right) with
    | Some l, Some r -> l + r
    | None, Some r -> r
    | Some l, None -> l
    | None, None -> 0

let sumOfParents row index = 
    let left = row |> List.tryItem (index - 1)
    let right = row |> List.tryItem (index)
    sum left right

let rec row rowNumber = 
    if rowNumber = 1 then
        [1]
    else
        let previousRow = row (rowNumber - 1)
        [0..rowNumber - 1] 
        |> List.map (fun index -> sumOfParents previousRow index)

let triangle rows = 
    [1..rows] 
    |> List.map row