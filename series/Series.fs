module Series

open System

let slices (input : string) length = 
    if length > input.Length then
        let msg = 
            "Slice length should not be longer than the input string"
        raise <| new ArgumentException(msg)
    else
        input 
        |> List.ofSeq
        |> List.map (string >> int)
        |> List.windowed length