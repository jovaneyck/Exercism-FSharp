module Accumulate

let accumulate f l = 
    let rec accRecursive acc l =
         match l with
         | [] -> acc
         | h :: t -> accRecursive (f h :: acc) t
    accRecursive [] l |> List.rev