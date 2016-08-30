module ProteinTranslation

open System

type Protein =
    | Methionine
    | Phenylalanine
    | Leucine
    | Serine
    | Tyrosine
    | Cysteine
    | Tryptophan
    | STOP //Not the cleanest bit of domain modeling here.

let translations = 
 [
    ("AUG", Methionine)
    ("UUU", Phenylalanine)
    ("UUC", Phenylalanine)
    ("UUA", Leucine)
    ("UUG", Leucine)
    ("UCU", Serine)
    ("UCC", Serine)
    ("UCA", Serine)
    ("UCG", Serine)
    ("UAU", Tyrosine)
    ("UAC", Tyrosine)
    ("UGU", Cysteine)
    ("UGC", Cysteine)
    ("UGG", Tryptophan)
    ("UAA", STOP)
    ("UAG", STOP)
    ("UGA", STOP)
 ]
 |> Map.ofList

let splitCodons (strand : string) =
    strand
    |> Seq.chunkBySize 3
    |> List.ofSeq
    |> List.map String.Concat

let translateCodon c =
    match translations |> Map.tryFind c with
    | None -> failwithf "Unknown codon: %s" c
    | Some codon -> codon

let isStop c = c = STOP

let toString = sprintf "%A"

let translate strand =
    strand 
    |> splitCodons
    |> List.map translateCodon
    |> List.takeWhile (not << isStop)
    |> List.map toString