#r @"packages\Unquote.3.1.1\lib\net45\Unquote.dll"
open Swensen.Unquote

let sortedLetters word = word |> List.ofSeq |> List.sort

test  <@ sortedLetters "tan" = (['a';'n';'t']) @>
