module Accumulate

let accumulate f l = 
    let rec accRecursive acc l =
         match l with
         | [] -> acc
         | h :: t -> accRecursive (acc @ [f h]) t
    accRecursive [] l