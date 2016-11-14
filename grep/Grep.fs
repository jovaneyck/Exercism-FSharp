module Grep

open System.IO

let contains pattern (text : string) = text.Contains(pattern)

let toLower (text : string) = text.ToLower()

let containsCaseInsensitive pattern text = contains (pattern |> toLower) (text |> toLower)

let matchesFullLine ignoreCase (pattern : string) (text : string) = 
    if ignoreCase then
        pattern.ToLower() = text.ToLower()
    else
        pattern = text

type Hit = {fileName : string; lineNumber : int; text : string}

let indexed lines =
    lines
    |> Seq.indexed
    |> Seq.map (fun (nb, line) -> (nb + 1, line))

let grepLines filter pattern (fileName, lines) = 
    lines
    |> indexed
    |> Seq.filter (fun (nb, line) -> filter pattern line)
    |> Seq.map (fun (nb, line) -> {fileName = fileName; lineNumber = nb; text = line})

let flagEnabled (flags : string) flag =
    flags.Contains(flag)

let grepWith reader pattern flags files = 
    let flag = flagEnabled flags

    //Clean up this messy composition :(
    let printer =
        let linePrinter =
            if flag "n" then
                fun {lineNumber = nb; text = text} -> sprintf "%d:%s" nb text
            else
                fun {text = line} -> line

        if flag "l" then
            fun {fileName = fileName} -> fileName
        elif files |> List.length > 1 then
            fun l -> sprintf "%s:%s" l.fileName (linePrinter l)
        else 
            linePrinter

    let filter =
        let fullLineFilter =
            let caseFilter =
                if flag "i" then
                    containsCaseInsensitive
                else
                    contains

            if flag "x" then
                (fun p t -> caseFilter p t && matchesFullLine (flag "i") p t)
            else
                caseFilter

        if flag "v" then
            fun p t -> not (fullLineFilter p t)
        else 
            fullLineFilter

    let hits =
        files
        |> List.map (fun f -> (f, reader f))
        |> Seq.collect (grepLines filter pattern)
        |> Seq.map printer
        |> Seq.map (fun line -> line + "\n")

    let result =
        if flag "l" then
            hits
            |> Seq.distinct
        else
            hits

    result
    |> String.concat ""
    
let readLines file = File.ReadLines(file)

let grep pattern flags files = grepWith readLines pattern flags files