module Grep

open System.IO

type Hit = {fileName : string; lineNumber : int; text : string}

//messy filter/printer composition :(
let contains pattern (text : string) = text.Contains(pattern)
let toLower (text : string) = text.ToLower()
let containsCaseInsensitive pattern text = contains (pattern |> toLower) (text |> toLower)
let matchesFullLine ignoreCase pattern text = 
    if ignoreCase then ( pattern |> toLower) = ( text |> toLower)
    else pattern = text

let buildFilter flag =
    let filter =
        if flag "x" then matchesFullLine (flag "i")
        elif flag "i" then containsCaseInsensitive
        else contains

    if flag "v" then fun p t -> not <| filter p t
    else filter

let printLine {text = text} = text
let printWithLineNumber {lineNumber = nb; text = text} = sprintf "%d:%s" nb text
let printFileName {fileName = fileName} = fileName

let buildPrinter flag nbFiles =
        if flag "l" then 
            printFileName
        else
            let linePrinter =
                if flag "n" then printWithLineNumber
                else printLine

            if nbFiles > 1 then
                fun l -> sprintf "%s:%s" l.fileName (linePrinter l)
            else 
                linePrinter

let indexed lines =
    lines
    |> Seq.indexed
    |> Seq.map (fun (nb, line) -> (nb + 1, line))

let grepLines filter pattern (fileName, lines) = 
    lines
    |> indexed
    |> Seq.filter (fun (nb, line) -> filter pattern line)
    |> Seq.map (fun (nb, line) -> {fileName = fileName; lineNumber = nb; text = line})

let flagEnabled (flags : string) flag = flags.Contains(flag)

let grepWith reader pattern flags files = 
    let flagChecker = flagEnabled flags
    let filter = buildFilter flagChecker
    let printer = buildPrinter flagChecker (files |> List.length )

    let hits =
        files
        |> List.map (fun f -> (f, reader f))
        |> Seq.collect (grepLines filter pattern)
        |> Seq.map printer
        |> Seq.map (fun line -> line + "\n")

    let resultLines =
        if flagChecker "l" then hits |> Seq.distinct
        else hits

    resultLines |> String.concat ""
    
let readLines file = File.ReadLines(file)

let grep pattern flags files = grepWith readLines pattern flags files