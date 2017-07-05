module Ledger

open System
open System.Globalization

type Entry = { date: DateTime; description: string; change: int }
type Locale = EnUs | NlNl

let mkEntry date description change = 
    { date = DateTime.Parse(date, CultureInfo.InvariantCulture); description = description; change = change }

let parseLocale = 
    function
    | "nl-NL" -> NlNl
    | "en-US" -> EnUs
    | unknown -> failwithf "Unknown locale: %s" unknown

let cultureInfo =
    function
    | NlNl -> new CultureInfo("nl-NL")
    | EnUs -> new CultureInfo("en-US")

let header locale =
    match locale with
    | EnUs -> "Date       | Description               | Change       "
    | NlNl -> "Datum      | Omschrijving              | Verandering  "
   
let dateFormat locale =
    match locale with
    | EnUs -> "MM\/dd\/yyyy"
    | NlNl -> "dd-MM-yyyy"

let symbolFor currency =
    match currency with
    | "USD" -> "$"
    | "EUR" -> "€"
    | unknown -> failwithf "Unknown currency: %s" unknown
     
let formatDate locale (date : DateTime) =     
    date.ToString(dateFormat locale)
      
let formatDescription (description : string) =
    if description.Length <= 25 then 
        description.PadRight(25)
    else 
        description.[0..21] + "..."   

let formatChange locale currency change : string =
    let c = float change / 100.0
    let symbol = symbolFor currency
    let culture = cultureInfo locale
    
    let abs (v : float) = System.Math.Abs(v)
    let padLeft (v : string) = v.PadLeft(13)
    let formattedValue (v : float) = v.ToString("#,#0.00", culture)
    
    match c < 0.0, locale with
    | true,  NlNl -> [      symbol ; " " ;  formattedValue c                     ] 
    | true,  EnUs -> ["(" ; symbol ;        formattedValue <| abs c         ; ")"]
    | false, NlNl -> [      symbol ; " " ;  formattedValue c                ; " "]
    | false, EnUs -> [      symbol ;        formattedValue c                ; " "]
    |> Seq.reduce (+)
    |> padLeft

let formatEntry locale currency entry =
    let date = formatDate locale entry.date
    let description = formatDescription entry.description
    let change = formatChange locale currency entry.change

    sprintf "%s | %s | %s" date description change

let formatLedger currency locale entries =
    let parsedLocale = parseLocale locale
    
    let header = 
        header parsedLocale
    let lines =
        entries
        |> List.sortBy (fun x -> x.date, x.description, x.change)
        |> List.map (formatEntry parsedLocale currency)

    header :: lines 
    |> String.concat "\n"