module Dominoes

type Domino = int * int
let orientations (a,b) =
    if a = b then [(a,b)]
    else [(a,b);(b,a)]
    
let rec distribute element list = 
    let possibleOrientations = orientations element
    match list with
    | []-> 
        possibleOrientations 
        |> List.map List.singleton
    | h :: t ->
        let asFirstElement = 
            possibleOrientations 
            |> List.map (fun element -> (element :: list))
        let interleaved =
            distribute element t
            |> List.map (fun d -> h :: d)
        List.append asFirstElement interleaved
                
let rec permutations l =
     match l with
     | [] -> [[]]
     | h :: t ->
        permutations t
        |> List.collect (distribute h)

let rec validSequence c =
    match c with
    | [] -> true
    | [_] -> true
    | (_,a) :: (b,c) :: t -> a = b && (validSequence ((b,c) :: t))

let validChain c =
    match c with
    | [] -> true
    | _ ->
        let (first,_) = List.head c
        let (_,last) = List.last c
        first = last && validSequence c

let canChain (dominoes : Domino list) = 
    permutations dominoes
    |> Seq.exists validChain