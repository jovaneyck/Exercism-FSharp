module Markdown

let split (separator : string) (text : string) = 
    text.Split([|separator|], System.StringSplitOptions.None)
    |> List.ofArray

let join (separator : string) parts =
    System.String.Join(separator, parts |> List.toSeq)

let (|TextSurroundedWith|_|) marker text =
    let splitted = split marker text

    match splitted with
    | before :: [surrounded] -> 
        Some(before, surrounded, "")
    | before :: surrounded :: rest -> 
        Some (before, surrounded, join marker rest)
    | _ -> 
        None

let rec replaceWithTag marker tagName line =
    let openingTag = sprintf "<%s>" tagName
    let closingTag = sprintf "</%s>" tagName
    match line with
    | TextSurroundedWith marker (before, surrounded, after) -> 
        before + openingTag + surrounded + closingTag + (replaceWithTag marker tagName after)
    | _ -> line

let emphasis = replaceWithTag "__" "em"
let italics = replaceWithTag "_" "i"

let listitem text = 
    sprintf "<li>%s</li>" text

let paragraph use_p_tag line =
    if use_p_tag then
        sprintf "<p>%s</p>" line
    else
        line

let handleHeaders (line : string) =
    let headerLevel = 
        line 
        |> Seq.takeWhile ((=) '#')
        |> Seq.length
    let remainder = line.Substring(headerLevel).TrimStart([|' '|])

    sprintf "<h%d>%s</h%d>" headerLevel remainder headerLevel

let appendTo (builder : System.Text.StringBuilder) (text : string) = 
    builder.Append(text) |> ignore

let parse (markdown: string) =
   let html = new System.Text.StringBuilder()
   let append = appendTo html
   let mutable isList = false

   markdown.Split('\n')
   |> Array.iter (fun line -> 
        if line.StartsWith("*") then
            if not isList then  
                append "<ul>"
                isList <- true

            let trimmed = line.[2..]   
            let use_p_tag = not (trimmed.StartsWith "__" || trimmed.StartsWith "_")

            trimmed
            |> emphasis
            |> italics
            |> (paragraph use_p_tag)
            |> listitem
            |> append
        elif line.StartsWith("#") then
            line
            |> handleHeaders
            |> append
        else 
            line
            |> emphasis
            |> italics
            |> (paragraph true)
            |> append)

   if isList then
       append "</ul>"

   html.ToString()