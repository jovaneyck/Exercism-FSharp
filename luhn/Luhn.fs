module Luhn

let checkDigit number = number % 10L

let toDigits number = 
    let rec digits number acc =
        if number = 0L then acc
        else digits (number / 10L) ((number % 10L) :: acc)
    digits number []

let addends number = 
    number 
    |> toDigits
    |> List.rev
    |> List.indexed
    |> List.map 
        (fun (idx, el) -> if idx % 2 = 0 then el else 2L*el)
    |> List.map 
        (fun addend -> if addend <= 9L then addend else addend - 9L)
    |> List.rev

let modulo d n = n % d

let checksum number =
    number
    |> addends
    |> List.sum
    |> modulo 10L

let valid number = number |> checksum |> modulo 10L = 0L
    
let create number = 
    let options = seq {for i in 0L..9L -> number * 10L + i}
    options |> Seq.find valid