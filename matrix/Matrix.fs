module Matrix

let parse = string >> System.Int32.Parse

let fromString (input : string) = 
    input.Split([|'\n'|])
    |> Array.mapi (
        fun rowNb line -> 
            line.Split([|' '|])
            |> Array.mapi (
                fun colNb rawNb -> 
                    ((rowNb, colNb), parse rawNb)))
    |> Array.collect id

let private getBy grouping matrix =
    matrix
    |> Array.groupBy grouping
    |> Array.map (fun (_,group) -> 
        group
        |> Seq.map (fun (_, cell) -> cell))

let rows matrix = getBy (fun ((r,_),_) -> r) matrix
let cols matrix = getBy (fun ((_,c),_) -> c) matrix