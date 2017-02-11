module RunLengthEncoding

let toString (length, character) = 
    match length with
    | 1 -> 
        [character]
    | _ -> 
        let lengthAsChars = length |> string |> List.ofSeq
        List.append lengthAsChars [character]

let rec toLengthsAndChars text =
    if Seq.isEmpty text then
        Seq.empty
    else
        let head = Seq.head text
        let isPartOfRun = (=) head
        let run = text |> Seq.takeWhile isPartOfRun
        let remainder = text |> Seq.skipWhile isPartOfRun
        let runLength = run |> Seq.length

        seq {
            yield (runLength, head)
            yield! toLengthsAndChars remainder
        }

let seqToString = Seq.map string >> String.concat ""

let encode text = 
    text
    |> toLengthsAndChars
    |> Seq.collect toString
    |> seqToString

let (|Run|_|) text =
    let m = System.Text.RegularExpressions.Regex.Match(text, "^(?<length>\d+)(?<char>.)(?<remainder>.*)")
    match m.Success with
    | false -> None
    | true -> 
        let length = m.Groups.["length"].Value |> System.Int32.Parse
        let character = m.Groups.["char"].Value |> char
        let remainder = m.Groups.["remainder"].Value
        Some <| Run ((length,character), remainder)

let (|SingleCharacter|_|) (text : string) =
    let fst = text |> Seq.head
    let rest = text.Substring(1)
    Some <| SingleCharacter (fst, rest)

let rec parseRuns acc text = 
    match text with
    | "" -> acc |> List.rev
    | Run (r, remainder) -> 
        parseRuns (r :: acc) remainder
    | SingleCharacter(c,remainder) -> 
        parseRuns ((1,c) :: acc) remainder
    | unknown -> 
        failwithf 
            "Single character should always match, but didn't on: %s" 
            unknown
    
let expand (length, char) = List.replicate length char

let decode (text : string) = 
    text
    |> (parseRuns [])
    |> Seq.collect expand
    |> seqToString