module CryptoSquare

open System
open System.Text.RegularExpressions

let strip text = Regex.Replace(text, "[^(\w)]", "")

let lowercase (text : string) = text.ToLower()

let normalizePlaintext text = 
    text
    |> strip
    |> lowercase

let size text = 
    text 
    |> normalizePlaintext 
    |> String.length
    |> float 
    |> Math.Sqrt 
    |> Math.Ceiling 
    |> int

let chunk numberOfChunks text = 
    text
    |> Seq.chunkBySize numberOfChunks
    |> Seq.map String

let plaintextSegments text = 
    text
    |> normalizePlaintext
    |> chunk (size text)

let charsToString (input : char seq) : string = 
    new String(input |> Seq.toArray)

let columns square = 
    square
    |> Seq.collect (fun row -> row |> Seq.indexed)
    |> Seq.groupBy fst
    |> Seq.map (fun (_, column) -> column |> Seq.map snd |> charsToString)    

let ciphertext text = 
    text
    |> plaintextSegments
    |> columns
    |> String.concat ""

let split text = 
    text
    |> Seq.splitInto (size text) 
    |> Seq.map String

let normalizeCiphertext text =
    text
    |> ciphertext
    |> split
    |> String.concat " "