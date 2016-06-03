module RNATranscription

let complements = 
        [('G', 'C');('C', 'G');('T', 'A');('A', 'U')]
        |> Map.ofSeq

let complement nucleotide = complements |> Map.find nucleotide

let toString = Seq.toArray >> System.String

let toRna strand = 
    strand 
    |> Seq.map complement
    |> toString