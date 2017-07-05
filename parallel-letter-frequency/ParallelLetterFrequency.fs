module ParallelLetterFrequency

let frequencyForText (text : string) =
    text.ToLower()
    |> Seq.filter System.Char.IsLetter
    |> Seq.countBy id

let frequency texts = 
    [for t in texts -> 
        async {
            return frequencyForText t
        }
    ]
    |> Async.Parallel
    |> Async.RunSynchronously 
    |> Seq.concat   
    |> Seq.groupBy fst
    |> Seq.map (fun (letter, occurences) -> (letter, occurences |> Seq.sumBy snd))
    |> Map.ofSeq