module SimpleLinkedList

type LinkedList<'a> =
    | Nil
    | Cons of 'a * LinkedList<'a>

let nil = Nil

let isNil =
    function
    | Nil -> true
    | _ -> false

let create element list = 
    Cons (element, list)

let datum =
    function
    | Cons(element, _) -> element
    | Nil -> failwith "no datum for Nil"

let next =
    function
    | Cons(_, tail) -> tail
    | Nil -> failwith "no next for Nil"

let toList linkedList =
    let rec tl acc list =
        match list with
        | Nil -> acc |> List.rev
        | Cons (h, t) -> tl (h :: acc) t
    tl [] linkedList

let fromList list = 
    let rec fl acc revlist =
        match revlist with
        | [] -> acc
        | h :: t  -> fl (Cons(h, acc)) t
    fl Nil (list |> List.rev)

let rec append element =
    function
    | Nil -> Cons(element, Nil)
    | Cons(h,t) -> Cons(h, append element t)
let rec reverse = 
    function
    | Nil -> Nil
    | Cons(h,t) -> append h (reverse t)