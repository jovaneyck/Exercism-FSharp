module Sieve

let rec sieve candidates primes =
    match candidates with
    | [] -> primes |> List.rev
    | h :: t -> 
        let nextCandidates =
            t |> List.filter (fun c -> c % h <> 0)
        sieve nextCandidates (h :: primes)

let primesUpTo n = sieve [2..n] [] 