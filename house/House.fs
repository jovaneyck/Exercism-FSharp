module House

let sentences =
    [
        "the house that Jack built"
        "the malt\nthat lay in "
        "the rat\nthat ate "
        "the cat\nthat killed "
        "the dog\nthat worried "
        "the cow with the crumpled horn\nthat tossed "
        "the maiden all forlorn\nthat milked "
        "the man all tattered and torn\nthat kissed "
        "the priest all shaven and shorn\nthat married "
        "the rooster that crowed in the morn\nthat woke "
        "the farmer sowing his corn\nthat kept "
        "the horse and the hound and the horn\nthat belonged to "
    ]

let sentencesForVerse v =
    sentences
    |> List.take v 
    |> List.rev

let rec toVerse = 
    function
    | [] -> ""
    | sentence :: t -> sentence + (toVerse t)

let rhyme = 
    [1..sentences.Length]
    |> List.map sentencesForVerse
    |> List.map toVerse
    |> List.map (sprintf "This is %s.")
    |> String.concat "\n\n"