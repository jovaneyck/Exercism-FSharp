module Minesweeper

type Content = Empty | Mine
type Coordinate = int * int
type Space = {Location : Coordinate; Content : Content}
type Annotated = {Space : Space; NeighbouringBombs : int}

let split (text : string) = text.Split([|'\n'|])

let parse board =
    let parseContent =
        function
        | '*' -> Mine
        | ' ' -> Empty
        | unknown -> failwithf "Unexpected space in the grid: %c" unknown

    let buildSpace rowNb colNb content =
        {Location = (rowNb, colNb); Content = content}

    board
    |> Seq.mapi 
        (fun rowNb line -> 
            line 
            |> Seq.mapi 
                (fun colNb content -> 
                    buildSpace rowNb colNb (parseContent content)))
    |> Seq.collect id

let neighbours board (x,y) =
    let deltas = [(-1, -1); (-1,0); (-1, 1)
                  (0, -1);          (0,1)
                  (1, -1);  (1, 0); (1,1)]
    let coordinates =
        deltas 
        |> Seq.map (fun (dx, dy) -> (x + dx, y + dy))
    board
    |> Seq.filter (fun space -> coordinates |> Seq.contains space.Location)

let neighbouringBombs board location =
    neighbours board location
    |> Seq.filter(fun space -> space.Content = Mine)
    |> Seq.length

let annotateBoard board =
    board
    |> Seq.map(fun space -> {Space = space; NeighbouringBombs = neighbouringBombs board space.Location})
  
let toStringSpace annotated = 
    match annotated.Space.Content, annotated.NeighbouringBombs with
    | Empty, 0 -> " "
    | Empty, nb -> string nb
    | Mine,_ -> "*"
    
let toStringRow row = 
    row 
    |> Seq.map toStringSpace
    |> String.concat ""

let toStringBoard annotatedBoard =
    let rows =  
        annotatedBoard 
        |> Seq.groupBy (fun a -> fst a.Space.Location)
        |> Seq.map snd
    rows
    |> Seq.map toStringRow
    |> String.concat "\n"

let annotate (board : string) = 
    board 
    |> split
    |> parse
    |> annotateBoard
    |> toStringBoard