type Customer = 
    {
        Name : string
        Age : int
    }

let bob = { Name = "Bob"; Age = 23 }

type HttpResult<'a> =
    | OK of Customer
    | ServerError of 'a

let result = ServerError 500

match result with
| OK {Name = n} -> ()
| ServerError message -> ()