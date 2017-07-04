module WordSearch

let indexed puzzle =
    puzzle
    |> List.mapi
        (fun rowNb row -> 
            row 
            |> Seq.toList 
            |> List.mapi 
                (fun colNb letter -> 
                    (colNb + 1, rowNb + 1), letter))
    |> List.collect id

let byRow ((column,row),_) = row
let byColumn ((column,row),_) = column

let horizontal puzzle =
    puzzle
    |> List.groupBy byRow
    |> List.map snd
    
let reverse lines =  List.map List.rev lines

let vertical puzzle =
    puzzle
    |> List.groupBy byColumn
    |> List.map snd

let diagonalsStartingFrom puzzle ((c,r),_) =
    puzzle
    |> List.filter (fun ((c2,r2),_) -> c - c2 = r - r2)

let diagonal puzzle =
    let startCells = 
        puzzle
        |> List.filter (fun ((c,r),_) -> c = 1 || r = 1)
    startCells
    |> List.map (diagonalsStartingFrom puzzle)

let mirrorDiagonalsStartingFrom puzzle ((c,r),_) =
    puzzle
    |> List.filter (fun ((c2,r2),_) -> c2 - c = r - r2)

let mirrorDiagonal puzzle =
    let maxColumn = 
        puzzle
        |> List.map (fun ((c,_),_) -> c)
        |> List.max
    let startCells = 
        puzzle
        |> List.filter (fun ((c,r),_) -> c = maxColumn || r = 1)

    startCells
    |> List.map (mirrorDiagonalsStartingFrom puzzle)

let orElse f v =
    match v with
    | None -> f ()
    | Some _ -> v

let indexes word line = 
    let toResult indexes =
        match indexes with
        | [] -> None
        | _ -> Some (List.last indexes, List.head indexes)

    let rec indexesOf acc word line =
        match word, line with
        | [], _ -> 
            toResult acc
        | _, [] -> 
            None
        | w :: ws, (c,l) :: ls when w = l -> 
            let result = indexesOf (c :: acc) ws ls
            result
            |> orElse 
                (fun () -> 
                    if List.isEmpty acc then
                        indexesOf acc word ls
                    else 
                        None)
        | ws, _ :: ls when (List.isEmpty acc) -> 
            indexesOf acc ws ls
        | _ -> 
            None
    indexesOf [] word line

let tryFind word puzzle lineBuilder =
    puzzle
    |> lineBuilder
    |> Seq.choose (word |> Seq.toList |> indexes)
    |> Seq.tryHead

let find puzzle word = 
    let input = puzzle |> indexed
    let directions = 
        [
            horizontal; 
            vertical;
            diagonal;
            mirrorDiagonal;
        ]
        |> List.collect (fun dir -> [dir; dir >> reverse])

    directions 
    |> Seq.tryPick (tryFind word input)