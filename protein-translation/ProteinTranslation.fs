module ProteinTranslation

type Protein =
    | Methionine
    | Phenylalanine
    | Leucine
    | Serine
    | Tyrosine
    | Cysteine
    | Tryptophan
    | STOP //Not the cleanest bit of modeling here.

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

let toString = sprintf "%A"

let rec translateCodons acc codons =
    match codons with
    | [] -> acc
    | c :: cs -> 
        match translations |> Map.tryFind c with
        | None -> failwithf "Unknown codon: %s" c
        | Some c ->
            match c with
            | STOP -> acc
            | translation -> translateCodons (translation :: acc) cs

let splitCodons (strand : string) =
    strand
    |> Seq.chunkBySize 3
    |> List.ofSeq
    |> (List.map (System.String.Concat))

let translate (strand : string) =
    strand 
    |> splitCodons
    |> translateCodons []
    |> List.rev
    |> List.map toString