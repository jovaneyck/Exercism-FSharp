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

let paragraph use_p_tag line =
    if use_p_tag then
        sprintf "<p>%s</p>" line
    else
        line

let handleHeaders (line : string) =
    //Not 100% behaviour-preserving, but YOLO
    let headerLevel = 
        line 
        |> Seq.takeWhile ((=) '#')
        |> Seq.length
    let remainder = line.Substring(headerLevel).TrimStart([|' '|])

    sprintf "<h%d>%s</h%d>" headerLevel remainder headerLevel

let parse (markdown: string) =
   let mutable html = ""
   let mutable isList = false

   let lines = markdown.Split('\n')

   for i = 0 to lines.Length - 1 do
       let line = lines.[i]
       if line.StartsWith("*") then
            if not isList then  
                html <- html + "<ul>"
                isList <- true

            let trimmed = lines.[i].[2..]   
            let use_p_tag = not (trimmed.StartsWith "__" || trimmed.StartsWith "_")

            let parsed =
                trimmed
                |> emphasis
                |> italics
                |> (paragraph use_p_tag)

            let listItem = sprintf "<li>%s</li>" parsed
            
            html <- html + listItem

       elif line.StartsWith("#") then
            let parsed = handleHeaders line
            html <- html + parsed
       else 
            let parsed =
                line
                |> emphasis
                |> italics
                |> (paragraph true)

            html <- html + parsed

   if isList then
       html <- html + "</ul>"

   html