module PerfectNumbers

type NumberType =
    | Abundant
    | Perfect
    | Deficient

let factorOf number factor = number % factor = 0

let factorsExcludingSelf number =
    [1..number/2]
    |> List.filter (factorOf number)

let aliquotSum = factorsExcludingSelf >> List.sum

let classify number = 
    let sum = aliquotSum number

    if sum = number then
        Perfect
    elif sum > number then
        Abundant
    else
        Deficient