module BankAccount

type AccountMessage =
    | UpdateBalance of float
    | GetBalance of AsyncReplyChannel<Option<float>>
    | Open
    | Close
type Agent<'T> = MailboxProcessor<'T>

type BankAccount() = 
    let account = new Agent<AccountMessage>(fun agent ->
        let rec loop balance =
            async {
                let! msg = agent.Receive()

                let next = 
                    match msg with
                    | Open ->
                        match balance with
                        | None -> (Some 0.0)
                        | b -> 
                            printfn "Error: tried to open an already open account."
                            b
                        |> loop
                    | Close -> loop None
                    | GetBalance c -> 
                        c.Reply balance
                        loop balance
                    | UpdateBalance b ->
                        match balance with
                        | None ->
                            printfn "Error: tried to update balance of a closed account."
                            loop None
                        | Some bal ->
                            loop <| Some (bal + b)
                return! next
            }
        loop None)
    do account.Start()

    member this.openAccount() = 
       account.Post Open
    member this.getBalance() = 
        let balance = 
            account.PostAndReply <| fun c -> GetBalance c
        match balance with 
        | None -> failwith "This account is not currently open, it has no balance yet."
        | Some b -> b
    member this.Balance = 
        account.PostAndReply <| fun c -> GetBalance c
    member this.updateBalance b = 
        account.Post (UpdateBalance b)
    member this.closeAccount() = 
        account.Post Close