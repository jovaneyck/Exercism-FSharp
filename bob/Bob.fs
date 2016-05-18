module Bob

let (|Silence|_|) (msg : string) = 
    if System.String.IsNullOrWhiteSpace msg 
    then Some Silence
    else None

let containsALetter (msg : string) =
    msg |> Seq.exists (System.Char.IsLetter)

let isAllCaps (msg : string) = msg.ToUpper() = msg

let isAShout (msg : string) = 
    containsALetter msg && isAllCaps msg

let (|Shouting|_|) msg = 
    if isAShout msg
    then Some Shouting
    else None

let (|Question|_|) (msg : string) =
    if msg.EndsWith("?") 
    then Some Question
    else None

let hey message = 
    match message with
    | Silence -> "Fine. Be that way!"
    | Shouting -> "Whoa, chill out!"
    | Question -> "Sure."
    | _ -> "Whatever."