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
    g 
    |> List.filter (function Node _ -> true | _ -> false)
    |> List.sort
let edges g = 
    g
    |> List.filter (function Edge _ -> true | _ -> false)
    |> List.sort

let attrs g = 
    g
    |> List.filter (function Attribute _ -> true | _ -> false)
    |> List.sort