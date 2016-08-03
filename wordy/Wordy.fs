module WordProblem

open FSharp.Text.RegexProvider //Let's play with this for a bit :)

type Term =
    | Constant of int
    | Plus of Term * Term
    | Minus of Term * Term
    | Multiply of Term * Term
    | Divide of Term * Term

let parseConstant = (int >> Constant)

type ExpressionRegex = Regex<"^(?<LeftCompositeOperand>.*) (?<Operator>.*?) (?<RightOperand>\-?\d*)$">

let toTerm left operator right = 
    match operator with 
        | "+" -> Some <| Plus(left, right)
        | "-" -> Some <| Minus(left, right)
        | "*" -> Some <| Multiply(left, right)
        | "/" -> Some <| Divide(left, right)
        | _   -> None

let rec parseOperation left operator right = 
    let rightOperand = right |> parseConstant

    left
    |> parseExpression 
    |> Option.bind (fun leftOperand -> toTerm leftOperand operator rightOperand)

and parseExpression expression : Term option = 
    match Regex.Match(expression, "^\-?\d*$").Success with
    | true -> 
        Some <| parseConstant expression
    | false ->
        let m = ExpressionRegex().TypedMatch(expression)
        match m.Success with
        | false -> None
        | true -> 
            parseOperation 
                m.LeftCompositeOperand.Value
                m.Operator.Value 
                m.RightOperand.Value

let tokenize (question : string) =
    //My regex-fu struggles with multi-word nested operations, so I collapse them
    question
        .Replace("multiplied by","*")
        .Replace("divided by", "/")
        .Replace("plus", "+")
        .Replace("minus", "-")

type QuestionRegex = Regex<"What is (?<Calculation>.*)\?">
let parseQuestion question = 
    let m = QuestionRegex().TypedMatch(question)
    match m.Success with
    | false -> None
    | _ -> parseExpression m.Calculation.Value
    
let rec reduce = 
    function
    | Constant(c) -> c
    | Plus(l, r) -> (reduce l) + (reduce r)
    | Minus(l, r) -> (reduce l) - (reduce r)
    | Multiply(l, r) -> (reduce l) * (reduce r)
    | Divide(l, r) -> (reduce l) / (reduce r)

let solve = tokenize >> parseQuestion >> (Option.map reduce)