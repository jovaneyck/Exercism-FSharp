module Deque

//Double linked list with amortized O(1) time pops/shifts.
type LinkedList = {fromFirst : int list; fromLast : int list}

let mkDeque = {fromFirst = List.empty; fromLast = List.empty}

let push value ({fromLast = l} as ll) = {ll with fromLast = value :: l}
let unshift value ({fromFirst = f} as ll) = {ll with fromFirst = value :: f}
   
let rec unshiftAndPossiblyFlip l r = 
    match (l, r) with
    | ([], []) -> failwith "cannot remove an item from an empty linked list"
    | ([], r) -> unshiftAndPossiblyFlip (r |> List.rev) [] 
    | (h :: t, r) -> (h, (t, r))

let rec shift {fromFirst = f; fromLast = l} = 
    let (shifted, (newF, newL)) = unshiftAndPossiblyFlip f l
    (shifted, {fromFirst = newF; fromLast = newL})

let rec pop {fromFirst = f; fromLast = l} = 
    let (popped, (newL, newF)) = unshiftAndPossiblyFlip l f
    (popped, {fromFirst = newF; fromLast = newL})