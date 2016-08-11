module Palindrome

let reverse number = 
    number 
    |> string 
    |> Seq.rev 
    |> Seq.map string
    |> String.concat "" 
    |> int 
let isPalindrome number =
    number = (number |> reverse)

let palindromesWithFactors lower upper = 
    seq {
        for f in [lower..upper] do
        for s in [f..upper] do
            let product = f*s
            if  isPalindrome product then
                yield (product,(f,s))
    }

let firstPalindromeBy sorter lower upper = 
    let (palindrome, palindromeAndFactors) = 
        palindromesWithFactors lower upper
        |> Seq.groupBy (fun (palindrome, _) -> palindrome)
        |> sorter
        |> Seq.head
    (palindrome, palindromeAndFactors |> Seq.map (fun (_, factors) -> factors))

let largestPalindrome  = firstPalindromeBy <| Seq.sortByDescending (fun (p, _) -> p)
let smallestPalindrome = firstPalindromeBy <| Seq.sortBy (fun (p, _) -> p)