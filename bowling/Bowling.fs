module Bowling

type Game = {rolls : int list}
type Frame =
    | Strike
    | Spare of int * int
    | Open of int * int
    //Domain language: are bonus rolls considered part of the last Strike/Spare frame?
    | BonusRollAfterSpare of int
    | BonusRollsAfterStrike of int * int

let newGame = { rolls = [] }
let roll pins game = 
    {game with rolls = game.rolls @ [pins] }

let toFrames rolls =
    let rec f acc rolls =
        match rolls with
        | [] -> Some <| List.rev acc
        | [10;b;c] -> f ((BonusRollsAfterStrike (b,c)) :: Strike :: acc) []
        | 10 :: rs -> f (Strike :: acc) rs
        | [a;b;c] when a + b = 10 -> f ((BonusRollAfterSpare c) :: (Spare (a,b)) :: acc) []
        | a :: b :: rs when a + b = 10 -> f ((Spare (a,b)) :: acc) rs
        | a :: b :: rs -> f ((Open (a,b)) :: acc) rs
        | _ -> None
    f [] rolls

let rec rolls frames =
    match frames with
    | [] -> []
    | Strike :: fs -> 10 :: (rolls fs)
    | BonusRollsAfterStrike(f,s) :: _ -> [f; s]
    | Spare(f,s) :: fs -> f :: s :: (rolls fs)
    | BonusRollAfterSpare b :: _ -> [b]
    | Open(f,s) :: fs -> f :: s :: (rolls fs)

let framePoints frames =
    let rec fp acc frames =
        match frames with
        | [] -> 
            acc
        | Strike :: fs -> 
            let nextTwo = rolls fs |> List.take 2 |> List.sum
            fp (acc + 10 + nextTwo) fs
        | Spare _ :: fs -> 
            let nextRoll = rolls fs |> List.head
            fp (acc + 10 + nextRoll) fs
        | BonusRollsAfterStrike _ :: _
        | BonusRollAfterSpare _ :: _ -> 
            fp acc []
        | Open(fst, snd) :: fs -> 
            fp (acc + fst + snd) fs
            
    fp 0 frames

let validate frames = 
    let validRoll r = r >= 0 && r <= 10
    let validRolls = Seq.forall validRoll
    let validFrame f =
        match f with
        | Strike -> 
            true
        | Open (a,b) -> 
            validRolls [a; b] && a + b <= 10
        | BonusRollsAfterStrike (a,b) -> 
            validRolls [a;b] && (a = 10 && b = 10 || a + b <= 10)
        | BonusRollAfterSpare b ->
            validRolls [b]
        | Spare (a,b) -> 
            validRolls [a;b] && a + b = 10
    let validFrames = Seq.forall validFrame
    let gameComplete frames = 
        let rec bonusRollsRolled =
            function
            | [] -> true
            | [Strike]
            | [Spare _] -> false
            | _ :: t -> bonusRollsRolled t

        frames |> Seq.length >= 10 && bonusRollsRolled frames

    if frames |> validFrames && frames |> gameComplete then
        Some frames
    else
        None

let score game = 
    game.rolls
    |> toFrames
    |> Option.bind validate
    |> Option.map framePoints