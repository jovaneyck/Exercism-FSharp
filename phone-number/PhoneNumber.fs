module PhoneNumber

open System

let parse nb = 
    nb
    |> List.ofSeq
    |> List.filter Char.IsDigit

let toString = Array.ofList >> String

let parsePhoneNumber number =
    let parsed = parse number 
    match (Seq.length parsed, parsed) with
    | 10, relevant
    | 11, '1' :: relevant ->
        relevant
        |> toString
        |> Some
    | _ -> None