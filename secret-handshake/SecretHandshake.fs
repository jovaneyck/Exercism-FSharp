module SecretHandshake

type Actions =
    | Action of string
    | ReverseOrder

let secrets =
    [
        (1, Action "wink")
        (2, Action "double blink")
        (4, Action "close your eyes")
        (8, Action "jump")
        (16, ReverseOrder)
    ]

let applyAction acc action = 
    match action with
    | Action a -> a :: acc
    | ReverseOrder -> acc |> List.rev

let foldActions = List.fold applyAction [] >> List.rev

let handshake number = 
    secrets
    |> List.filter (fun (code, action) -> code &&& number <> 0)
    |> List.map (fun (_, action) -> action)
    |> foldActions