module Rectangle

type Element = 
    | Corner
    | HorizontalLine
    | VerticalLine
    | Empty
type Location = {x : int; y : int}
type Point = {location : Location; element : Element}

let parseCharacter =
    function
    | '+' -> Corner
    | ' ' -> Empty
    | '-' -> HorizontalLine
    | '|' -> VerticalLine
    |  _  -> failwithf "Unrecognized input"

let parse input =
    input
    |> List.map List.ofSeq
    |> List.mapi 
        (fun rowNb row -> 
            row 
            |> List.mapi (fun colNb element -> 
                {location = {x = colNb; y = rowNb}; element = parseCharacter element}))
    |> List.collect id

let isCorner {element = e} = e = Corner

let atLowerRightOf a b = a.x < b.x && a.y < b.y

let pointsSpanning allCorners upperLeft lowerRight =
    let upperRight = 
        allCorners 
        |> Seq.tryFind (fun c -> 
            let x = lowerRight.location.x
            let y = upperLeft.location.y
            c.location = {x = x; y = y})
    let lowerLeft = 
        allCorners 
        |> Seq.tryFind (fun c -> 
            let x = upperLeft.location.x
            let y = lowerRight.location.y
            c.location = {x = x; y = y})
    match upperRight, lowerLeft with
    | Some upperRight, Some lowerLeft -> Some (upperLeft, upperRight, lowerLeft, lowerRight)
    | _ -> None

let isHorizontalConnection = 
    function
    | HorizontalLine
    | Corner -> true
    | _ -> false

let isVerticalConnection = 
    function
    | VerticalLine
    | Corner -> true
    | _ -> false

let connected points (upperLeft, upperRight, lowerLeft, lowerRight) = 
    let topLineOK = 
        [(upperLeft.location.x + 1) .. (upperRight.location.x - 1)] 
        |> List.forall (fun x -> points |> List.exists (fun p -> (p.location = {x = x; y = upperLeft.location.y}) && isHorizontalConnection p.element))
    let bottomLineOK = 
        [(lowerLeft.location.x + 1) .. (lowerRight.location.x - 1)] 
        |> List.forall (fun x -> points |> List.exists (fun p -> (p.location = {x = x; y = lowerLeft.location.y}) && isHorizontalConnection p.element))
    let leftLineOK = 
        [(upperLeft.location.y + 1) .. (lowerLeft.location.y - 1)] 
        |> List.forall (fun y -> points |> List.exists (fun p -> (p.location = {x = upperLeft.location.x; y = y}) && isVerticalConnection p.element))
    let rightLineOK = 
        [(upperRight.location.y + 1) .. (lowerRight.location.y - 1)] 
        |> List.forall (fun y -> points |> List.exists (fun p -> (p.location = {x = upperRight.location.x; y = y}) && isVerticalConnection p.element))
    topLineOK && bottomLineOK && leftLineOK && rightLineOK

let rectanglesStartingAtCorner points otherCorners corner = 
    let diagonals =
        otherCorners
        |> List.filter (fun c -> atLowerRightOf corner.location c.location)
    let possibleRectangles = 
        diagonals
        |> List.choose (fun d -> pointsSpanning otherCorners corner d)
    possibleRectangles
    |> List.filter (connected points)

let getRectangles points = 
    let corners = points |> List.filter isCorner
    corners 
    |> List.collect (fun c -> rectanglesStartingAtCorner points (corners |> List.except [c]) c)

let rectangles input =
    input
    |> parse
    |> getRectangles
    |> Seq.length