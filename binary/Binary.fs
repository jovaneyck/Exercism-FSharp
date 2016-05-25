module Binary

let private add first second = 
    match first, second with
    | Some f, Some s -> Some (f + s)
    | _ -> None

let private convert (input : string) =
    input
    |> Seq.rev
    |> Seq.mapi
        (fun index digit ->
            match digit with
            | '0'           -> Some 0
            | '1'           -> Some <| pown 2 index
            | invalid       -> None)
    |> Seq.reduce add

let orDefault theDefault value = 
    value 
    |> defaultArg <| theDefault

let toDecimal input  = 
    convert input
    |> orDefault 0