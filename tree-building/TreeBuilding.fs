module TreeBuilding

type Record = { RecordId: int; ParentId: int }
type Tree = 
    | Branch of int * Tree list
    | Leaf of int

let directChildrenOf records parent =
    records
    |> List.filter(fun r -> r.ParentId = parent.RecordId)
    |> List.sortBy (fun r -> r.RecordId)

let root records = 
    records 
    |> List.tryFind (fun r -> r.RecordId = 0 && r.ParentId = -1)

let rec buildSubTree records node =
    match directChildrenOf records node with
    | [] -> Leaf node.RecordId
    | children -> 
        let leaves = (children |> List.map (buildSubTree records))
        Branch (node.RecordId, leaves)

let rec size tree = 
    match tree with
    | Leaf _ -> 1
    | Branch (_, children) -> 1 + (children |> List.map size |> List.sum)

let containsInvalidRecord records =
    records |> Seq.exists (fun r -> r.RecordId <= r.ParentId)

let hasAllContinuousIds records =
    let expectedIds = [0..(records |> Seq.length) - 1]
    let actualIds =
        records 
        |> List.map (fun r -> r.RecordId)
        |> List.sort
    expectedIds = actualIds

let buildTree records = 
    if List.isEmpty records then failwith "Empty input."
    if not <| hasAllContinuousIds records then failwith "Non-continuous record ids found."
    if containsInvalidRecord records then failwith "Invalid record found."

    match root records with
    | None -> failwith "Did not find a valid root node."
    | Some r -> 
        let tree = buildSubTree records r
        if size tree <> (Seq.length records) then failwith "Cycles detected."
        tree