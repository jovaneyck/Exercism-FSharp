module Allergies

type Allergen =
    | Peanuts
    | Cats
    | Strawberries
    | Eggs
    | Shellfish
    | Chocolate
    | Pollen
    | Tomatoes

//Order is important here
let allergens = [Eggs; Peanuts; Shellfish; Strawberries; Tomatoes; Chocolate; Pollen; Cats]

let toBinary score = 
    let rec decode acc score =
        if score = 0 then acc
        else 
            let digit = score % 2
            let remainder = score / 2
            decode (digit::acc) remainder
    decode [] score
    
let allergies score =
    toBinary score
    |> List.rev
    |> Seq.zip allergens
    |> Seq.filter (fun (_, allergic) -> allergic = 1)
    |> Seq.map (fun (allergen, _) -> allergen)

let allergicTo ingredient score = 
    allergies score |> Seq.contains ingredient