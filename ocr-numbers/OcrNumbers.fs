module OcrNumbers

let translations = 
    [   
        (
            " _ " + "\n" +
            "| |" + "\n" +
            "|_|" + "\n" +
            "   ",
            "0"
        )
        (
            "   " + "\n" +
            "  |" + "\n" +
            "  |" + "\n" +
            "   ",
            "1"
        )
        (
            " _ " + "\n" +
            " _|" + "\n" +
            "|_ " + "\n" +
            "   ",
            "2"
        )
        (
            " _ " + "\n" +
            " _|" + "\n" +
            " _|" + "\n" +
            "   ",
            "3"
        )
        (
            "   " + "\n" +
            "|_|" + "\n" +
            "  |" + "\n" +
            "   ",
            "4"
        )
        (
            " _ " + "\n" +
            "|_ " + "\n" +
            " _|" + "\n" +
            "   ",
            "5"
        )
        (
            " _ " + "\n" +
            "|_ " + "\n" +
            "|_|" + "\n" +
            "   ",
            "6"
        )
        (
            " _ " + "\n" +
            "  |" + "\n" +
            "  |" + "\n" +
            "   ",
            "7"
        )
        (
            " _ " + "\n" +
            "|_|" + "\n" +
            "|_|" + "\n" +
            "   ",
            "8"
        )
        (
            " _ " + "\n" +
            "|_|" + "\n" +
            " _|" + "\n" +
            "   ",
            "9"   
        )
    ]
    |> Map.ofSeq

let splitLines (text : string) = text.Split('\n')
let toLetterBlocks lines =
    lines
    |> Seq.map (Seq.chunkBySize 3)
    |> Seq.collect Seq.indexed
    |> Seq.groupBy fst
    |> Seq.map snd
    |> Seq.map (fun grouping -> grouping |> Seq.map (fun (_, chars) -> chars))

let concatChars (chars : char seq) = System.String.Concat(chars)
let toStringRepresentation block =
    block
    |> Seq.map concatChars
    |> String.concat "\n"

let translate block = 
    match translations |> Map.tryFind block with
    | Some digit -> digit
    | None -> "?"

let translateRow lines =
    let blocks = lines |> toLetterBlocks
    blocks
    |> Seq.map toStringRepresentation
    |> Seq.map translate
    |> String.concat ""

let convert input = 
    let lines = input |> splitLines
    let rows = lines |> Seq.chunkBySize 4
    rows
    |> Seq.map translateRow
    |> String.concat ","