module Tournament

type Outcome = Win | Loss | Draw
type Match = 
    { First : string; Second : string; Outcome : Outcome }
type Stats = 
    {Wins : int; Draws : int; Losses : int}
    member this.MatchesPlayed = this.Draws + this.Losses + this.Wins
    member this.Points = 3 * this.Wins + 1 * this.Draws

let split (line : string) =
    line.Split([|';'|]) |> Array.toList

let parseOutcome outcome =
    match outcome with
    | "win" -> Some <| Win
    | "loss" -> Some <| Loss
    | "draw" -> Some <| Draw
    | _ -> None

let parse result =
    match split result with
    | [teamA; teamB; outcome] -> 
        outcome
        |> parseOutcome
        |> Option.map 
            (fun o -> {First = teamA; Second = teamB; Outcome = o})    
    | _ -> None

let initialStats = {Wins = 0; Losses = 0; Draws = 0}
let orDefault v d = defaultArg d v
let updateStats stats match' = 
    let firstStats =
        stats
        |> Map.tryFind match'.First
        |> orDefault initialStats
    let secondStats =
        stats
        |> Map.tryFind match'.Second
        |> orDefault initialStats
    match match'.Outcome with
    | Win ->
        stats
        |> Map.add match'.First {firstStats with Wins = firstStats.Wins + 1}
        |> Map.add match'.Second {secondStats with Losses = secondStats.Losses + 1}
    | Loss ->
        stats
        |> Map.add match'.First {firstStats with Losses = firstStats.Losses + 1}
        |> Map.add match'.Second {secondStats with Wins = firstStats.Wins + 1}
    | Draw ->
        stats
        |> Map.add match'.First {firstStats with Draws = firstStats.Draws + 1}
        |> Map.add match'.Second {secondStats with Draws = firstStats.Draws + 1}

let sorted (stats : (string * Stats) list) = 
    query {
        for (name, stat) in stats do
        sortByDescending stat.Points
        thenBy name
        select (name, stat)
    }
    |> List.ofSeq
        
let toStats =
    List.fold updateStats Map.empty >> Map.toList

let header = "Team                           | MP |  W |  D |  L |  P"
let toTableRow (team : string, (stats : Stats)) = 
    sprintf
        "%s|  %d |  %d |  %d |  %d |  %d"
        (team.PadRight 31)
        stats.MatchesPlayed
        stats.Wins
        stats.Draws
        stats.Losses
        stats.Points

let tally results = 
    results
    |> List.choose parse
    |> toStats
    |> sorted
    |> List.map toTableRow
    |> List.append [header]