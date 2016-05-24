module Binary

let private without substr (str : string) =
    str.Replace(substr, "")

let private isValidBinary (input : string) =
    let invalidChars = 
        input 
        |> without "1" 
        |> without "0"
    invalidChars 
    |> Seq.isEmpty

let private convert (input : string) =
    input
    |> Seq.map (string >> float)
    |> Seq.rev
    |> Seq.indexed
    |> Seq.map 
        (fun (idx, digit) -> 
            let n = idx |> float
            digit * 2.0 ** n)
    |> Seq.sum
    |> int

let toDecimal input  = 
    if not (isValidBinary input) then 0
    else convert input