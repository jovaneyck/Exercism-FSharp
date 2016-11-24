module CircularBuffer

type Buffer<'a> = {elements : 'a list; size : int}

let mkCircularBuffer size = 
    {elements = []; size = size}

let private isFull b =
    b.elements |> List.length = b.size 

let write c b =
    if isFull b
    then failwithf "Cannot write to a full buffer."
    else {b with elements = List.append b.elements [c]} //YOLO

let read b = 
    match b.elements with
    | h :: t -> (h, {b with elements = t})
    | [] -> failwith "Cannot read, this buffer is empty"

let clear b = 
    {b with elements = []}

let replaceOldestElementWith c b = 
    let (_, withoutOldest) = read b
    write c withoutOldest

let forceWrite c b = 
    if not <| isFull b then write c b
    else replaceOldestElementWith c b