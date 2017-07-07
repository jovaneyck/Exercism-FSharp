module BracketPush

let matched brackets = 
    let rec matchedRec stack list = 
        match list, stack with
        | [], [] -> true
        | '[' :: bs, _          -> matchedRec ('[' :: stack) bs
        | ']' :: bs, '[' :: ss  -> matchedRec ss bs
        | ']' :: _, _           -> false

        | '{' :: bs, _          -> matchedRec ('{' :: stack) bs
        | '}' :: bs, '{' :: ss  -> matchedRec ss bs
        | '}' :: _, _           -> false

        | '(' :: bs, _          -> matchedRec ('(' :: stack) bs
        | ')' :: bs, '(' :: ss  -> matchedRec ss bs
        | ')' :: _, _           -> false
        
        | _ :: bs, _            -> matchedRec stack bs

        | _                     -> false

    let list = brackets |> Seq.toList
    matchedRec [] list