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
    List.foldBack create list Nil

let reverse list = 
    let rec rev acc =
        function
        | Nil -> acc
        | Cons(h,t) -> rev (create h acc) t
    rev Nil list