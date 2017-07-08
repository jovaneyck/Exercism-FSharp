module Sublist

type Equality =
    | Equal
    | Sublist
    | Superlist
    | Unequal

let rec prefix a b =
    match a, b with
    | [], _ -> true
    | a :: aas, b :: bbs when a = b -> prefix aas bbs
    | _ -> false

let rec isSublist a b = 
    match a, b with
    | [], _ -> true
    | _, [] -> false
    | a, b :: bbs -> prefix a (b :: bbs) || isSublist a bbs
    
let isSuperlist a b = isSublist b a

let sublist a b =
    if a = b                then Equal
    elif isSublist a b      then Sublist
    elif isSuperlist a b    then Superlist
    else                         Unequal