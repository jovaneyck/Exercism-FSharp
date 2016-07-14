module NucleoTideCount

let nucleotides = ['A';'C';'T';'G']
let emptyCount = 
    nucleotides
    |> List.map (fun n -> (n,0)) 
    |> Map.ofList

let nucleotideCounts strand = 
    strand
    |> Seq.countBy id
    |> Seq.fold 
        (fun acc (nucleotide, count) -> acc |> Map.add nucleotide count) 
        emptyCount
  
let valid nucleotide = 
    List.contains nucleotide nucleotides

let count nucleotide strand = 
    if not (valid nucleotide) then
        failwithf 
            "Invalid nucleotide: %c" 
            nucleotide
    else
        nucleotideCounts strand 
        |> Map.find nucleotide
