module RobotName

open System

let rng = new System.Random()

let randomChar() = 
    'A'
    |> int
    |> (fun a -> a + rng.Next(0,25))
    |> char

let randomNb() = rng.Next(0,10)

let randomName() =
    sprintf
        "%c%c%d%d%d"
        (randomChar ())
        (randomChar ())
        (randomNb ())
        (randomNb ())
        (randomNb ())

type Robot() = 
    let mutable name = randomName()
    member this.Name = name
    member this.Reset() = 
        name <- randomName()