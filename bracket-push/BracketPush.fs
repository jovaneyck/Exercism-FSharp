module BracketPush

let matched brackets = 
    let pairs = 
        [('[',']')
         ('{','}')
         ('(',')')]
    let opens = pairs |> List.map fst
    let closes = pairs |> List.map snd
    let openBracketFor =
        pairs
        |> List.map (fun (o,c) -> (c,o))
        |> Map.ofList

    let (|Open|_|) character = 
        if opens |> List.contains character then
            Some (Open character)
        else
            None
    let (|Close|_|) character = 
        if closes |> List.contains character then
            Some (Close character)
        else
            None

    let openingBracketFor closingBracket = 
        openBracketFor |> Map.find closingBracket

    let rec matchedRec stack list = 
        match list, stack with
        | [], []                                                -> true
        | Open o :: bs, _                                       -> matchedRec (o :: stack) bs
        | Close c :: bs, o :: ss when o = openingBracketFor c   -> matchedRec ss bs
        | Close _ :: _, _                                       -> false
        | _ :: bs, _                                            -> matchedRec stack bs
        | _                                                     -> false

    let list = brackets |> Seq.toList
    matchedRec [] list