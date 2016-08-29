module ErrorHandling

let handleErrorByThrowingException () =
    failwith "An exception"

let handleErrorByReturningOption value =
    match value |> System.Int32.TryParse with
    | true, parsed -> Some parsed
    | _ -> None

type Result<'a> = 
    | Ok of 'a
    | Error of string

let handleErrorByReturningResult value = 
    match value |> System.Int32.TryParse with
    | true, parsed -> Ok parsed
    | _ -> Error "Could not convert input to integer"

let bind f x = 
    match x with
    | Ok result -> f result
    | error -> error

let cleanupDisposablesWhenThrowingException resource =
    use r = resource
    raise <| System.Exception()