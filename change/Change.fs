module Change

let rec findChange amount denominations =
    if amount < 0 then
        None
    elif amount = 0 then
        Some <| [[]]
    else
        match denominations with
        | [] -> None
        | d :: ds when d <= amount -> 
            let ignored = (findChange amount ds) 
            let used = (findChange (amount - d) (d::ds))

            match ignored, used with
            | None, None -> None
            | None, Some b -> Some (b |> List.map(fun r -> d :: r))
            | s, None -> s
            | Some a, Some b -> Some (a @ (b |> List.map(fun r -> d :: r)))
        | _ :: ds -> findChange amount ds    

let change amount denominations = 
    let sortedDenominations = denominations |> List.sortDescending
    let coins = findChange amount sortedDenominations
    coins  
    |> Option.map (List.minBy List.length >> List.sort)