module NthPrime

//Literal sieve implementation using an infinite stream
// not the fastest stuff out there.
//8sec for 1.000th, ~infinity for 10.000th
let primes = 
    let rec primesRec candidates =
        seq {
            let nextPrime = candidates |> Seq.head
            yield nextPrime

            let remainingCandidates = 
                candidates 
                |> Seq.tail
                |> Seq.filter (fun el -> el % nextPrime <> 0)   
            yield! primesRec remainingCandidates
        }
    let initialCandidates = Seq.initInfinite <| (+) 2
    primesRec initialCandidates

let nthPrime n = primes |> Seq.item (n - 1)