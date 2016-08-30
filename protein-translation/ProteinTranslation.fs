module ProteinTranslation

open System

let splitCodons (strand : string) =
    strand
    |> Seq.chunkBySize 3
    |> List.ofSeq
    |> List.map String

let translateCodon = 
    function
    | "AUG" ->  "Methionine"
    | "UUU"
    | "UUC" -> "Phenylalanine"
    | "UUA" 
    | "UUG" ->  "Leucine"
    | "UCU"
    | "UCC"
    | "UCA"
    | "UCG" -> "Serine"
    | "UAU"
    | "UAC" -> "Tyrosine"
    | "UGU"
    | "UGC" -> "Cysteine"
    | "UGG" -> "Tryptophan"
    | "UAA"
    | "UAG"
    | "UGA" -> "STOP"
    | c     -> failwithf "Unknown codon: %s" c

let isStop c = c = "STOP"

let translate strand =
    strand 
    |> splitCodons
    |> List.map translateCodon
    |> List.takeWhile (not << isStop)