module ScaleGenerator

let capitalize (text : string) : string = 
    match text |> Seq.toList with
    | [] -> ""
    | h :: t ->
        h.ToString().ToUpper() + (System.String.Concat(t))
    
let sharpPitches = ["A"; "A#"; "B"; "C"; "C#"; "D"; "D#"; "E"; "F"; "F#"; "G"; "G#"]
let tonicsThatUseFlats = ["F"; "Bb"; "Eb"; "Ab"; "Db"; "d"; "g"; "c"; "f"; "bb"]
let flatPitches = ["A"; "Bb"; "B"; "C"; "Db"; "D"; "Eb"; "E"; "F"; "Gb"; "G"; "Ab"]

let repeatingSharps = 
    Seq.replicate 3 sharpPitches 
    |> Seq.concat

let repeatingFlats = 
    Seq.replicate 3 flatPitches 
    |> Seq.concat

let step =
    function
    | 'm' -> 1
    | 'M' -> 2
    | 'A' -> 3
    | unknown -> failwithf "Unknown interval: %c" unknown

let toSteps intervals = 
    intervals
    |> Seq.toList
    |> List.map step

let pitchesAfter pitches tonic =
    pitches 
    |> Seq.skipWhile (fun n -> n <> tonic)
    |> Seq.skip 1
    |> List.ofSeq

let pitchesFor tonic =
    if tonicsThatUseFlats |> List.contains tonic then
        repeatingFlats
    else
        repeatingSharps

let rec recPitches acc intervals notes = 
    match intervals, notes with
    | [_], _ -> acc |> List.rev
    | 1 :: is, n :: ns -> recPitches (n :: acc) is ns
    | 2 :: is, _ :: n :: ns -> recPitches (n :: acc) is ns
    | 3 :: is, _ :: _ :: n :: ns -> recPitches (n :: acc) is ns
    | _ -> failwithf "I can't handle %A - %A" intervals notes    

let pitches tonic intervals = 
    let startingPitch = capitalize tonic
    let pitches = pitchesFor tonic
    let steps = toSteps intervals
    let nextPitches = pitchesAfter pitches startingPitch
    recPitches [] steps nextPitches
    |> List.append [startingPitch]