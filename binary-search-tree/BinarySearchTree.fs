module BinarySearchTree

type Tree =
    | Nil
    | Node of NodeDescriptor
and NodeDescriptor =
    {left : Tree; value: int; right : Tree}

let singleton value = Node({left = Nil; value = value; right = Nil})

let value tree =
    match tree with
    | Node({value = v}) -> v
    | Nil -> failwith "Nil has no value"

let rec insert value tree =
    match tree with
    | Nil -> singleton value
    | Node({value = v; left = l; right = r} as n) ->
        if value <= v then
            Node({n with left = insert value l})
        else
            Node({n with right = insert value r})

let left tree =
    match tree with
    | Nil -> None
    | Node({left = l}) -> Some l

let right tree =
    match tree with
    | Nil -> None
    | Node({right = r}) -> Some r

let fromList l =
    let rec from list acc =
        match list with
        | [] -> acc
        | h :: t -> from t (acc |> insert h)
    from l Nil

let rec toList t =
    match t with
    | Nil -> []
    | Node({left = l; value = v; right = r}) ->
        (toList l) @ [v] @ (toList r)
