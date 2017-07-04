module Bowling

type Game = {rolls : int list}
type Frame =
    | Strike
    | Spare of int * int
    | Regular of int * int
    | BonusRollAfterSpare of int

let newGame = { rolls = [] }
let roll pins game = 
    {game with rolls = game.rolls @ [pins] }

let frames rolls =
    let rec f acc rolls =
        match rolls with
        | [] -> Some <| List.rev acc
        | 10 :: rs -> f (Strike :: acc) rs
        | [a;b;c] when a + b = 10 -> f ((BonusRollAfterSpare c) :: (Spare (a,b)) :: acc) []
        | a :: b :: rs when a + b = 10 -> f ((Spare (a,b)) :: acc) rs
        | a :: b :: rs -> f ((Regular (a,b)) :: acc) rs
        | _ -> None
    f [] rolls

let rec rolls frames =
    match frames with
    | [] -> []
    | Strike :: fs -> 10 :: (rolls fs)
    | Spare(f,s) :: fs -> f :: s :: (rolls fs)
    | BonusRollAfterSpare b :: fs -> b :: (rolls fs)
    | Regular(f,s) :: fs -> f :: s :: (rolls fs)

let framePoints frames =
    let rec fp acc frames =
        match frames with
        | [] -> 
            acc
        | BonusRollAfterSpare _ :: _ -> 
            fp acc []
        | Strike :: fs -> 
            let nextTwo = rolls fs |> List.take 2 |> List.sum
            fp (acc + 10 + nextTwo) fs
        | Spare _ :: fs -> 
            let nextRoll = rolls fs |> List.head
            fp (acc + 10 + nextRoll) fs
        | Regular(fst, snd) :: fs -> 
            fp (acc + fst + snd) fs
            
    fp 0 frames

let score game = 
    game.rolls
    |> frames
    |> Option.map framePoints