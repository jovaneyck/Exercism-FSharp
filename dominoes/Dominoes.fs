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
            let (firstDominoValue,_) = h
            for orientation in possibleOrientations do 
                let (_,dominoValueOfNext) = orientation
                if dominoValueOfNext = firstDominoValue then
                    yield (Seq.append (Seq.singleton orientation) list)

            for d in distribute element t do
                yield Seq.append (Seq.singleton h) d
    }
                
let rec permutations l =
     if l |> Seq.isEmpty 
     then Seq.singleton Seq.empty
     else
        let h = Seq.head l
        let t = Seq.tail l
        permutations t
        |> Seq.collect (distribute h)

let rec validSequence sequence =
    match sequence with
    | [] -> true
    | [_] -> true
    | (_,a) :: (b,c) :: t -> a = b && (validSequence ((b,c) :: t))

let validChain chain =
    match chain with
    | [] -> true
    | _ ->
        let (first,_) = List.head chain
        let (_,last) = List.last chain
        first = last && validSequence chain

let canChain (dominoes : Domino list) = 
    dominoes
    |> permutations
    |> Seq.map Seq.toList
    |> Seq.exists validChain