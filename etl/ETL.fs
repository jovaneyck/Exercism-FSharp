module ETL

open System

let private toLower (s : String) = s.ToLower()

let transform input = 
    input
    |> Map.toSeq
    |> Seq.collect 
        (fun (score, letters) -> 
            letters 
            |> Seq.map (fun letter -> (letter |> toLower, score)))
    |> Map.ofSeq