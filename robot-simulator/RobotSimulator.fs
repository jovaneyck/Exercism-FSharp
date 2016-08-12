module RobotSimulator

type Bearing =
    | North 
    | East
    | South
    | West
type Coordinate = int * int
type Robot = {Bearing : Bearing; Position : Coordinate}
type Instruction =
    | Left
    | Right
    | Advance

let createRobot bearing coord = {Bearing = bearing; Position = coord}

let right =
    function
    | North -> East
    | East -> South
    | South -> West
    | West -> North
let left =
    function
    | North -> West
    | West -> South
    | South -> East
    | East -> North

let turn shift robot = {robot with Bearing = (shift robot.Bearing)}
let turnRight = turn right
let turnLeft = turn left

let step (x,y) bearing =
    match bearing with
    | North -> (x, y + 1)
    | East -> (x + 1, y)
    | South -> (x, y - 1)
    | West -> (x - 1, y)

let advance robot = {robot with Position = (step robot.Position robot.Bearing)}

let parse = 
    function
    | 'R' -> Right
    | 'L' -> Left
    | 'A' -> Advance
    | invalid -> failwithf "Invalid instruction: %c" invalid

let findAction instruction =
    match instruction with
    | Right -> turnRight
    | Left -> turnLeft
    | Advance -> advance

let handleInstruction robot instruction =
    let action = findAction instruction
    action robot

let simulate robot instructions = 
    instructions
    |> Seq.map parse
    |> Seq.fold handleInstruction robot