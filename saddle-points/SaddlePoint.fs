module SaddlePoints

type Cell = (int * int) * int

let toCells matrix : Cell list = 
    matrix
    |> List.mapi 
        (fun rnumber row -> 
            row 
            |> List.mapi 
                (fun cnumber cell -> 
                    ((rnumber, cnumber), cell)))
    |> List.collect id


let largerThanAll values value = value >= (values |> List.max)
let smallerThanAll values value = value <= (values |> List.min)

let isSaddlePoint allPoints point =
    let ((r,c), value) = point
    let otherPoints = allPoints |> List.except [point]

    let valuesOfCellsMatching predicate = otherPoints |> List.filter predicate |> List.map snd
    let valuesInSameRow = valuesOfCellsMatching (fun ((row,_),_) -> r = row)
    let valuesInSameColumn = valuesOfCellsMatching (fun ((_,column), _) -> c = column)

    (value |> largerThanAll valuesInSameRow) && (value |> smallerThanAll valuesInSameColumn)

let saddlePoints matrix = 
    let cells = matrix |> toCells
    cells
    |> List.filter (isSaddlePoint cells)
    |> List.map (fun (coord, _) -> coord)