module LeapYear

let private divisibleBy divisor nb = 
    nb % divisor = 0
let private (|DivisibleBy|_|) d nb = 
    if nb |> divisibleBy d
    then Some DivisibleBy 
    else None

let isLeapYear year = 
    match year with
    | DivisibleBy 400 -> true
    | DivisibleBy 100 -> false
    | DivisibleBy 4   -> true
    | _               -> false