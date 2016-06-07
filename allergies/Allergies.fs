module Allergies

open System

[<Flags>]
type Allergen =
    | Eggs = 1
    | Peanuts = 2
    | Shellfish = 4
    | Strawberries = 8
    | Tomatoes = 16
    | Chocolate = 32
    | Pollen = 64
    | Cats = 128
   
let allergicTo allergen score = 
    (score |> enum<Allergen>).HasFlag(allergen)

let allergies score =
    Enum.GetValues(typeof<Allergen>)
    |> Seq.cast<Allergen>
    |> Seq.filter (fun allergen -> allergicTo allergen score)