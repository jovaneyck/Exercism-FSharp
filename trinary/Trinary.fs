module Trinary

let private add first second = 
    match first, second with
    | Some f, Some s -> Some (f + s)
    | _ -> None

let private convert (input : string) =
    input
    |> Seq.rev
    |> Seq.mapi
        (fun index digit ->
            let multiplier = pown 3 index
            match digit with
            | '0'           -> Some <| 0 * multiplier
            | '1'           -> Some <| 1 * multiplier
            | '2'           -> Some <| 2 * multiplier
            | invalid       -> None)
    |> Seq.reduce add

let orDefault theDefault value = 
    value 
    |> defaultArg <| theDefault

let toDecimal input  = 
    convert input
    |> orDefault 0