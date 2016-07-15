module TwelveDaysSong

let ordinals = 
    [
        "first"
        "second"
        "third"
        "fourth"
        "fifth"
        "sixth"
        "seventh"
        "eighth"
        "ninth"
        "tenth"
        "eleventh"
        "twelfth"
    ]

let gifts = 
    [
        "a Partridge in a Pear Tree"
        "two Turtle Doves"
        "three French Hens"
        "four Calling Birds"
        "five Gold Rings"
        "six Geese-a-Laying"
        "seven Swans-a-Swimming"
        "eight Maids-a-Milking"
        "nine Ladies Dancing"
        "ten Lords-a-Leaping"
        "eleven Pipers Piping"
        "twelve Drummers Drumming"
    ]
    
let ordinalsByDay = 
    ordinals
    |> Seq.mapi (fun index ordinal -> (index + 1, ordinal))
    |> Map.ofSeq

let ordinalFor day = ordinalsByDay |> Map.find day

let allGiftsFor day = gifts |> List.take day

let join (sep : string) (strings : string seq) = 
    System.String.Join(sep, strings)

let giftTextFor day =
    match allGiftsFor day with
    | [] -> ""
    | [single] -> single
    | h :: t -> join ", " (("and " + h ::t) |> List.rev)

let verse day = 
    sprintf 
        "On the %s day of Christmas my true love gave to me, %s.\n" 
        (ordinalFor day) 
        (giftTextFor day)

let verses from until = 
    [from..until]
    |> Seq.map verse
    |> Seq.map (fun v -> v + "\n")
    |> Seq.reduce (+)

let song = verses 1 12