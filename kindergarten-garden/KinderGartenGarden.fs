module KinderGartenGarden

open System

type Plant =
    | Clover
    | Radishes
    | Grass
    | Violets

type Child = string
type Garden = Map<Child,Plant seq>

let defaultChildren = 
    [
        "Alice"; 
        "Bob"; 
        "Charlie"; 
        "David";
        "Eve"; 
        "Fred"; 
        "Ginny"; 
        "Harriet"; 
        "Ileana"; 
        "Joseph"; 
        "Kincaid"; 
        "Larry"
    ]

let split (s : string) = 
    s.Split([|'\n'|], StringSplitOptions.None)
    |> Seq.map (fun line -> seq {for letter in line do yield letter})

let parse plant =
    match plant with
    | 'C' -> Clover
    | 'R' -> Radishes
    | 'G' -> Grass
    | 'V' -> Violets
    | unknown -> failwith <| sprintf "Unknown plant: %c" unknown

let garden children description : Garden = 
    let parsedPlants =
        description
        |> split
        |> Seq.map (fun line -> line |> Seq.map parse)
    let plantsPerChild =
        parsedPlants
        |> Seq.map (Seq.chunkBySize 2)
        |> Seq.collect Seq.indexed
        |> Seq.groupBy (fun (index,_) -> index)
        |> Seq.map (fun (_, v) -> v)
        |> Seq.map (fun g -> g |> Seq.collect (fun (_,p) -> p ))

    plantsPerChild
    |> Seq.zip children
    |> Map.ofSeq

let defaultGarden description = 
    garden defaultChildren description

let lookupPlants child garden = 
    match garden |> Map.tryFind child with
    | Some plants -> plants
    | None -> Seq.empty