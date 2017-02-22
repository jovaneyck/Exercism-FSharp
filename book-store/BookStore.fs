module BookStore

type Book = int
type Basket = Book list
type Set = Book list
type Combination = Set list

let rec except element list =
    match list with
    | [] -> []
    | x :: xs when x = element -> xs
    | x :: xs -> x :: (except element xs)

let combinationsWith (book : Book) (combination : Combination) : Combination list = 
    combination
    |> List.map 
        (fun set -> 
            if List.contains book set then
                None
            else
                let other = combination |> except set
                Some <| (book :: set) :: other)
    |> List.choose id

let rec combinations (books : Basket) : Combination list =
    match books with
    | [] -> [[]]
    | b :: bs ->
        let withoutBook = combinations bs
        let withBookAsSeparate = withoutBook |> List.map (fun r -> [b] :: r)
        let withBookMixedInGroups = withoutBook |> List.collect (combinationsWith b)
        List.append withBookAsSeparate withBookMixedInGroups

let setPrice (set : Set) = 
    let nbBooks = List.length set
    let unitPrice = 8m
    let discount =
        match nbBooks with
        | 2 -> 0.05m
        | 3 -> 0.10m
        | 4 -> 0.20m
        | 5 -> 0.25m
        | _ -> 0m
    let discountFactor = 1m - discount

    (decimal nbBooks) * unitPrice * discountFactor
    
let price (combination : Combination) = 
    combination
    |> List.sumBy setPrice
    
let calculateTotalCost (books : Basket) = 
    books
    |> combinations
    |> List.map price
    |> List.min