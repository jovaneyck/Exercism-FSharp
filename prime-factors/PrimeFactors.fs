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

let factorsFor factors number =
    let rec f acc number =
        if number =1L then 
            acc |> List.rev
        else
            let factor =
                factors
                |> Seq.find (fun p -> number % p = 0L)
            f (factor :: acc) (number / factor) 
    f [] number

let primeFactorsFor = factorsFor primes