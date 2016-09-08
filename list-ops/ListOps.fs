module ListOps

let length l =
    let rec lngth acc =
        function
        | [] -> acc
        | _ :: t -> lngth (acc + 1) t
    lngth 0 l

let reverse l = 
    let rec rev acc = 
        function
        | [] -> acc
        | x :: xs -> rev (x :: acc) xs
    rev [] l

let map f l =
    let rec mapr acc =
        function
        | [] -> acc |> reverse
        | x :: xs -> mapr (f x :: acc) xs
    mapr [] l

let filter f l = 
    let rec filt acc =
        function
        | [] -> acc |> reverse
        | x :: xs when f x -> filt (x :: acc) xs
        | _ :: xs -> filt acc xs
    filt [] l

let rec foldl f seed = 
    function
    | [] -> seed
    | x :: xs -> foldl f (f seed x) xs

let rec foldr f seed l =
    foldl (fun a b -> f b a) seed (reverse l)

let append xs ys = 
    let rec apprev x y =
        match x with
        | [] -> y
        | h :: t -> apprev t (h :: y)
    apprev (reverse xs) ys

let concat l = 
    let rec conc acc l = 
        match l with
        | [] -> reverse acc
        | h :: t -> conc (append (reverse h) acc) t
    conc [] l