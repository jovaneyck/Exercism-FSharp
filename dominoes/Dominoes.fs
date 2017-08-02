module Dominoes

type Domino = int * int

let orientations (a,b) =
    seq {
        if a = b then yield (a,b)
        else 
            yield (a,b)
            yield (b,a)
    }
    
let rec distribute element list = 
    let possibleOrientations = orientations element
    seq {
        if list |> Seq.isEmpty 
        then
            yield! possibleOrientations |> Seq.map Seq.singleton
        else
            let h = Seq.head list
            let t = Seq.tail list
            let asFirstElement = 
                possibleOrientations 
                |> Seq.map (fun element -> (Seq.append (Seq.singleton element) list))
            yield! asFirstElement
            let interleaved =
                distribute element t
                |> Seq.map (fun d -> Seq.append (Seq.singleton h) d)
            yield! interleaved
    }
    
                
let rec permutations l =
     if l |> Seq.isEmpty 
     then Seq.singleton Seq.empty
     else
        let h = Seq.head l
        let t = Seq.tail l
        permutations t
        |> Seq.collect (distribute h)

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
    dominoes
    |> permutations
    |> Seq.map Seq.toList
    |> Seq.exists validChain