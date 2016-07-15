module Sieve

let rec sieve candidates acc =
    match candidates with
    | [] -> acc
    | h :: t -> 
        let nextCandidates =
            t 
            |> List.filter (fun c -> c % h <> 0)
        sieve nextCandidates (h :: acc)
let primesUpTo n = 
    sieve [2..n] [] 
    |> List.rev