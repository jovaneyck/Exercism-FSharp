module DotDsl

type Attribute = { name: string; value: string }
type Node = {name : string; attributes : Attribute list}
type Edge = {start : string; stop : string; attributes : Attribute list}
type Element =
    | Node of Node
    | Edge of Edge
    | Attribute of Attribute
    
type Graph = Element list

let graph elements : Graph = 
    elements

let buildAttribute (name,value) = 
    {name = name; value = value}
let attr name value = 
    Attribute <| buildAttribute (name,value)

let node name attributes = 
    Node {name = name; attributes = (attributes |> List.map buildAttribute) }

let edge start stop attributes = 
    Edge {start = start; stop = stop; attributes = (attributes |> List.map buildAttribute)}

let nodes g = 
    let rec nodes' acc g = 
        match g with
        | [] -> 
            acc
            |> List.sortBy (fun n -> n.name)
            |> List.map Node
        | Node n :: t -> nodes' (n :: acc) t
        | _ :: t -> nodes' acc t
    nodes' [] g
let edges g = 
    let rec edges' acc g = 
        match g with
        | [] -> 
            acc
            |> List.sortBy (fun e -> e.start)
            |> List.map Edge
        | Edge e :: t -> edges' (e :: acc) t
        | _ :: t -> edges' acc t
    edges' [] g

let attrs g = 
    let rec attrs' acc g = 
            match g with
            | [] -> 
                acc
                |> List.sortBy (fun (a : Attribute) -> a.name)
                |> List.map Attribute
            | Attribute a :: t -> attrs' (a :: acc) t
            | _ :: t -> attrs' acc t
    attrs' [] g