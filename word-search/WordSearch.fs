module WordSearch

let indexed puzzle =
    puzzle
    |> List.mapi
        (fun rowNb row -> 
            row 
            |> Seq.toList 
            |> List.mapi (fun colNb col -> (colNb + 1, rowNb + 1), col))
    |> List.collect id

let rows puzzle =
    puzzle
    |> List.groupBy (fun ((c,r),_) -> r)
    |> List.map snd

let reverseRows puzzle =
    puzzle
    |> rows
    |> List.map (List.rev)

let indexes word line = 
    let toResult indexes =
        match indexes with
        | [] -> None
        | _ -> Some (List.last indexes, List.head indexes)

    let rec indexesOf acc word line =
        match word, line with
        | [], _ -> toResult acc
        | _, [] -> None
        | w :: ws, (c,l) :: ls when w = l -> indexesOf (c :: acc) ws ls
        | ws, _ :: ls when (List.isEmpty acc) -> indexesOf acc ws ls
        | _ -> None
    indexesOf [] word line

let findIn lineBuilder word puzzle =
    puzzle
    |> lineBuilder
    |> Seq.choose (word |> Seq.toList |> indexes)
    |> Seq.tryHead

let orElse f v =
    match v with
    | None -> f
    | Some _ -> v

let find puzzle word = 
    let input = puzzle |> indexed
    (findIn rows word input)
    |> (orElse (findIn reverseRows word input))