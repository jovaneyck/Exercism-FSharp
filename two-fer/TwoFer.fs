module TwoFer

let flip f x y = f y x
let orDefault = flip defaultArg

let getResponse name = 
    name
    |> orDefault "you"
    |> (sprintf "One for %s, one for me.")