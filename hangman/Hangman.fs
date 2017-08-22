module Hangman

type Progress = Busy of int | Win | Lose
type State = {progress : Progress; maskedWord : string; guesses : Set<char>}
type Game(secretWord : string) = 
    let initialState = {progress = Busy 9; maskedWord = System.String('_', secretWord.Length); guesses = Set.empty}
    
    let gameWon state = not <| state.maskedWord.Contains("_")
    let updateGuesses guess state = { state with guesses = (state.guesses |> Set.add guess)}
    let updateMaskedWord state = 
        let toMaskedLetter letter = if state.guesses |> Set.contains letter then string letter else "_"
        let newMasked : string = 
            secretWord
            |> Seq.map toMaskedLetter
            |> String.concat ""
        {state with maskedWord = newMasked}
    let updateProgress isCorrectGuess state = 
        match state.progress with
        | Win
        | Lose -> state
        | Busy 1 -> 
            {state with 
                progress = 
                    if not isCorrectGuess then Lose
                    else Win}
        | Busy failuresToAllow ->
            {state with 
                progress = 
                    if gameWon state then Win
                    elif not isCorrectGuess then Busy (failuresToAllow - 1)
                    else Busy failuresToAllow }
                
    let updateState state guess = 
        let isCorrectGuess = secretWord.Contains (string guess) && not (state.guesses |> Set.contains guess)
        state
        |> (updateGuesses guess)
        |> updateMaskedWord
        |> (updateProgress isCorrectGuess)

    //Input streams
    let startStateStream = new Event<State>()
    let guessStream = new Event<char>()
    
    //Output stream
    let gameStates = 
        Observable.merge
            startStateStream.Publish //to have the initial state in the stream on game start
            (guessStream.Publish |> Observable.scan updateState initialState) //game loop

    member this.Start () = startStateStream.Trigger initialState
    member this.MakeGuess guess = guessStream.Trigger guess
    member this.States = gameStates

let createGame word = new Game(word)
let startGame (game : Game) = game.Start ()
let statesObservable (game : Game) = game.States
let makeGuess letter (game : Game) = game.MakeGuess letter