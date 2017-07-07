module RailFenceCipher

let rec railIndices nbRails =
    seq {
         let oneWave = [0..nbRails-1] @ [nbRails-2..(-1)..1]
         for element in oneWave do yield element
         yield! railIndices nbRails
    }

let encode nbRails (text : string) = 
    let indices = railIndices nbRails
    let tokensIndexedByRail = text |> Seq.zip indices
    let rails = tokensIndexedByRail |> Seq.groupBy fst
    rails 
    |> Seq.collect (fun (_,tokens) -> tokens |> Seq.map (snd >> string))
    |> String.concat ""

let amountOfTokensPerRail nbRails indices = 
    [0..nbRails - 1]
    |> Seq.map (fun railNb -> indices |> Seq.filter(fun i -> i = railNb) |> Seq.length)
    |> Seq.toList

let rec split (tokensPerRail : int list) message =
    match tokensPerRail with
    | [] -> []
    | t :: ts -> (message |> Seq.take t) :: (split ts (message |> Seq.skip t))

let readRails rails indices =
    let railsMap = rails |> Seq.indexed |> Map.ofSeq

    let readRailsf (text,rails) index  = 
        let railToReadFrom = rails |> Map.find index
        let newMessage = sprintf "%s%c" text (railToReadFrom |> Seq.head)
        let newRails = rails |> Map.add index (railToReadFrom |> Seq.tail)
        (newMessage, newRails)

    let (text, _) = indices |> Seq.fold readRailsf ("", railsMap)
    text

let decode nbRails message = 
    let messageLength = message |> Seq.length
    let indices = railIndices nbRails |> Seq.take messageLength
    let nbTokensPerRail = amountOfTokensPerRail nbRails indices
    let rails = split nbTokensPerRail message
    readRails rails indices