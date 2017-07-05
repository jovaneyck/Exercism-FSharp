module SimpleCipher

let toIndex char = (int char) - (int 'a')

let toChar index = (int 'a') + index |> char

let shifts key = key |> Seq.map toIndex

let increase (offset, character) = 
    let index = toIndex character
    let newIndex = (index + offset) % 26
    toChar newIndex

let decrease (offset, character) = 
    let index = toIndex character
    let newIndex = 
        if offset > index then
            26 - (offset - index)
        else
            index - offset
    toChar newIndex

let valid key = 
    let nonEmpty = 
        key |> Seq.length > 0
    let allLowerLetters =
        key |> Seq.forall System.Char.IsLower
    nonEmpty && allLowerLetters

let reduceString characters = System.String.Concat<char> characters

let shift shifter key (text : string) =
    if not (valid key) then
        invalidArg "key" "Invalid key provided."
    else
        let shifts = shifts key
        text
        |> Seq.zip shifts
        |> Seq.map shifter
        |> reduceString

let encode key text = shift increase key text
let decode key text = shift decrease key text

let rng = new System.Random()

let randomChar () =
    rng.Next(int 'a', int 'z' + 1) |> char

let randomKey () =
    Seq.init 100 (fun _ -> randomChar ())
    |> reduceString

let encodeRandom plaintext = 
    let key = randomKey()
    (key, encode key plaintext)
