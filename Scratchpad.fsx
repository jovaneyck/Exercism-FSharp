#r @"packages\Unquote.3.1.1\lib\net45\Unquote.dll"
open Swensen.Unquote

open System

let naturalNumbers = seq{ for i in 2 .. Int32.MaxValue do yield i }
let rec primesIn nb =
    seq {
        let next = nb |> Seq.head
        yield next
        yield! primesIn (nb |> Seq.filter(fun n -> n % next <> 0))
    }

let primes = primesIn naturalNumbers

let factors n = 
    let rec f n acc =
        if n < 2 then acc
        else
            let smallestFactor = primes |> Seq.find (fun p -> n % p = 0)
            f (n / smallestFactor) (smallestFactor :: acc)
    f n [] |> List.rev

test <@ factors 2 = [2] @>
test <@ factors 3 = [3] @>
test <@ factors 4 = [2;2] @>
test <@ factors 6 = [2;3] @>