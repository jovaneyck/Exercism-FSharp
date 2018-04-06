module CollatzConjecture

let next number = 
    if number % 2 = 0 then
        number / 2
    else 
        1 + 3 * number

let steps (number: int): int option = 
    let rec stepsAcc acc n =
        match n with
        | n when n <= 0 -> None
        | n when n = 1 -> Some acc
        | n -> 
            let nextValue = next n
            stepsAcc (acc + 1) nextValue
    stepsAcc 0 number