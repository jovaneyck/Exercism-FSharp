module FoodChain

let cons list element = element :: list
let combine = List.reduce (fun x y -> x + "\n" + y)

let objectSwallowed =
    function
    | 1 -> "fly"
    | 2 -> "spider"
    | 3 -> "bird"
    | 4 -> "cat"
    | 5 -> "dog"
    | 6 -> "goat"
    | 7 -> "cow"
    | 8 -> "horse"
    | unknown -> failwithf "I don't know the object swallowed for verse: %d" unknown

let swallowed verseNumber =
    sprintf "I know an old lady who swallowed a %s." <| objectSwallowed verseNumber

let comment = 
    function
    | 1 -> None
    | 2 -> Some "It wriggled and jiggled and tickled inside her."
    | 3 -> Some "How absurd to swallow a bird!";
    | 4 -> Some "Imagine that, to swallow a cat!"
    | 5 -> Some "What a hog, to swallow a dog!"
    | 6 -> Some "Just opened her throat and swallowed a goat!"
    | 7 -> Some "I don't know how she swallowed a cow!"
    | _ -> None

let reason verseNumber =
    match verseNumber with
    | 1 -> 
        "I don't know why she swallowed the fly. Perhaps she'll die."
    | 3 -> 
        "She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her."
    | _ -> 
        sprintf 
            "She swallowed the %s to catch the %s." 
            (objectSwallowed verseNumber) 
            (objectSwallowed <| verseNumber - 1)

let verse number = 
    match number with
    | 8 ->
        [swallowed number;
        "She's dead, of course!"] 
        |> combine
    | _ ->
        let swallowed = [swallowed number]
        let description = Option.fold cons [] (comment number)
        let reasons = [number..(-1)..1] |> List.map reason

        swallowed
        @ description
        @ reasons
        |> combine

let song = 
    [1..8] 
    |> List.map verse 
    |> String.concat "\n\n"