module AllYourBase

open System

let toDecimal baseValue digits =
    let add baseValue one other = 
        one * baseValue + other

    digits
    |> List.reduce (add baseValue)

let toBase baseValue decimal = 
    let rec tobase acc decimal =
        if decimal = 0 then
            acc
        else
            let nextDigit = decimal % baseValue
            tobase (nextDigit :: acc) (decimal / baseValue)
    tobase [] decimal

let validDigits baseValue digits = 
    let containsDigits = not << List.isEmpty
    let hasLeadingZeroes =
        function
        | 0 :: _ -> true
        | _ -> false
    let positiveDigits = List.forall (fun el -> el >= 0)
    let allDigitsInBase b = List.forall (fun d -> d < b)

    [
        containsDigits
        not << hasLeadingZeroes
        positiveDigits
        allDigitsInBase baseValue
    ]
    |> List.forall (fun rule -> rule digits)

let validbase b = b > 1

let validInput digits inputbase outputbase = 
    validDigits inputbase digits
    && validbase inputbase
    && validbase outputbase

let convert inputbase digits outputbase = 
    digits
    |> toDecimal inputbase
    |> toBase outputbase

let rebase inputbase digits outputbase = 
    if not <| validInput digits inputbase outputbase then
        None
    else
        Some (convert inputbase digits outputbase)