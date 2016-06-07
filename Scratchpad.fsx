#r @"packages\Unquote.3.1.1\lib\net45\Unquote.dll"
open Swensen.Unquote

let decode score = 
    let rec decode acc score =
        if score = 0 then acc
        else 
            let digit = score % 2
            let remainder = score / 2
            decode (digit::acc) remainder
    decode [] score

test  <@ decode 1 = [1]@>
test  <@ decode 2 = [1;0]@>
test  <@ decode 4 = [1;0;0]@>
test  <@ decode 6 = [1;1;0]@>
test  <@ decode 7 = [1;1;1]@>

