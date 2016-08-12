module PrimeFactors

let primes = 
    let rec primesRec candidates =
        seq {
            let nextPrime = candidates |> Seq.head
            yield int64 nextPrime

            let remainingCandidates = 
                candidates 
                |> Seq.tail
                |> Seq.filter (fun el -> el % nextPrime <> 0)   
            yield! primesRec remainingCandidates
        }
    let initialCandidates = Seq.initInfinite <| (+) 2
    primesRec initialCandidates

let primeFactorsFor number =
    let rec factors acc number =
        if number = 1L then 
            acc |> List.rev
        else
            let factor =
                primes
                |> Seq.find (fun p -> number % p = 0L)
            factors (factor :: acc) (number / factor)
    factors [] number