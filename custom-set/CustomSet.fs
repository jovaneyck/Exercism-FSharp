module CustomSet

type Set<'a> = { Elements : 'a list }

let empty = 
    { Elements = [] }
let isEmpty { Elements = el } = 
    el = []
let singleton value = 
    { Elements = [value] }
let contains value {Elements = el} = 
    el 
    |> List.contains value
let fromList list = 
    let uniqueSorted = 
        list 
        |> List.distinct 
        |> List.sort
    { Elements = uniqueSorted }
let isSubsetOf {Elements = l} r = 
    l 
    |> List.forall (fun v -> contains v r)
let isDisjointFrom { Elements = l} r = 
    l 
    |> List.exists (fun v -> contains v r) 
    |> not
let insert value { Elements = e} = 
    fromList (value :: e)
let intersection { Elements = l } r = 
    l 
    |> List.filter (fun v -> contains v r)
    |> fromList
let difference {Elements = l} r = 
    l
    |> List.filter (fun v -> not <| contains v r)
    |> fromList
let union {Elements = l} {Elements = r} = 
    l
    |> List.append r
    |> fromList