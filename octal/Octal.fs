module Octal

let private add first second = 
    match first, second with
    | Some f, Some s -> Some (f + s)
    | _ -> None

let (|ADigitInBase|_|) b number =
    match number |> string |> System.Int32.TryParse with
    | false, _-> None
    | true, nb ->
        if 0 <= nb && nb < b
        then Some <| ADigitInBase nb
        else None

let private convert (input : string) =
    let b = 8
    input
    |> Seq.rev
    |> Seq.mapi
        (fun index charDigit ->
            match charDigit with
            | ADigitInBase b digit  ->  
                let multiplier = pown b index
                Some <| digit * multiplier
            | invalid -> None)
    |> Seq.reduce add

let orDefault theDefault value = 
    value 
    |> defaultArg <| theDefault

let toDecimal input  = 
    convert input
    |> orDefault 0