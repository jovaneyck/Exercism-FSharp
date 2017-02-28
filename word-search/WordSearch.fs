module WordSearch

let indexed puzzle =
    puzzle
    |> List.mapi
        (fun rowNb row -> 
            row 
            |> Seq.toList 
            |> List.mapi 
                (fun colNb col -> 
                    (colNb + 1, rowNb + 1), col))
    |> List.collect id

//-->
let horizontal puzzle =
    puzzle
    |> List.groupBy (fun ((c,r),_) -> r)
    |> List.map snd

let reverse lines =  List.map List.rev lines

//<--
let reverseHorizontal puzzle =
    puzzle
    |> horizontal
    |> reverse
//|
//v
let vertical puzzle =
    puzzle
    |> List.groupBy (fun ((c,r),_) -> c)
    |> List.map snd

//^
//|
let reverseVertical puzzle =
    puzzle
    |> vertical
    |> reverse

let diagonalsStartingFrom puzzle ((c,r),_) =
    puzzle
    |> List.filter (fun ((c2,r2),_) -> c - c2 = r - r2)

// \
//  v
let diagonal puzzle =
    let startCells = 
        puzzle
        |> List.filter (fun ((c,r),_) -> c = 1 || r = 1)
    startCells
    |> List.map (diagonalsStartingFrom puzzle)

//^
// \
let reverseDiagonal puzzle =
    puzzle
    |> diagonal
    |> reverse

let mirrorDiagonalsStartingFrom puzzle ((c,r),_) =
    puzzle
    |> List.filter (fun ((c2,r2),_) -> c2 - c = r - r2)

// /
//v
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

//  ^
// /
let  reverseMirrorDiagonal puzzle =
    puzzle
    |> mirrorDiagonal
    |> reverse

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
        | [], _ -> toResult acc
        | _, [] -> None
        | w :: ws, (c,l) :: ls when w = l -> 
            //EEeeeeewww, clean me up
            let result = indexesOf (c :: acc) ws ls
            result
            |> orElse (fun () -> 
                if List.isEmpty acc then
                    indexesOf acc word ls
                else 
                    None)

        | ws, _ :: ls when (List.isEmpty acc) -> indexesOf acc ws ls
        | _ -> None
    indexesOf [] word line

let tryFind lineBuilder word puzzle =
    puzzle
    |> lineBuilder
    |> Seq.choose (word |> Seq.toList |> indexes)
    |> Seq.tryHead

let find puzzle word = 
    let input = puzzle |> indexed
    let find direction = tryFind direction word input
    //TODO: clean up duplication here and in direction definitions
    None
    |> (orElse (fun () -> find horizontal))
    |> (orElse (fun () -> find reverseHorizontal))
    |> (orElse (fun () -> find vertical))
    |> (orElse (fun () -> find reverseVertical))
    |> (orElse (fun () -> find diagonal))
    |> (orElse (fun () -> find reverseDiagonal))
    |> (orElse (fun () -> find mirrorDiagonal))
    |> (orElse (fun () -> find reverseMirrorDiagonal))