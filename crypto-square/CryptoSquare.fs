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

let elementAtOrNothing index sequence = 
    match sequence |> Seq.tryItem index with
    | None -> ""
    | Some letter -> letter |> string

let elementsInColumn square columnNb =
    square 
    |> Seq.map (fun row -> row |> elementAtOrNothing columnNb)

let columns square = 
    let nbColumns = 
        square 
        |> Seq.head 
        |> String.length

    [0 .. nbColumns - 1]
    |> Seq.collect (elementsInColumn square)

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