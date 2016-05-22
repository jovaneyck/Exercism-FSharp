module BankAccountTest

open NUnit.Framework
open BankAccount

[<Test>]
let ``Has no balance when not open`` () =
    let account = BankAccount()
    Assert.That(account.Balance, Is.EqualTo(Option<float>.None))

[<Test>]
let ``Returns empty balance after opening`` () =
    let account = BankAccount()

    account.openAccount() |> ignore

    Assert.That(account.getBalance(), Is.EqualTo(0.0))

[<Test>]
let ``Check basic balance`` () =
    let account = BankAccount()

    account.openAccount() |> ignore

    let openingBalance = account.getBalance()

    account.updateBalance(10.0)

    let updatedBalance = account.getBalance()

    Assert.That(openingBalance, Is.EqualTo(0.0))
    Assert.That(updatedBalance, Is.EqualTo(10.0))

[<Test>]
let ``Balance can increment or decrement`` () =
    let account = BankAccount()

    account.openAccount() |> ignore

    let openingBalance = account.getBalance()

    account.updateBalance(10.0)

    let addedBalance = account.getBalance()

    account.updateBalance(-15.0)

    let subtractedBalance = account.getBalance()

    Assert.That(openingBalance, Is.EqualTo(0.0))
    Assert.That(addedBalance, Is.EqualTo(10.0))
    Assert.That(subtractedBalance, Is.EqualTo(-5.0))

[<Test>]
let ``Account can be closed`` () =
    let account = BankAccount()

    account.openAccount() |> ignore

    account.closeAccount()

    Assert.That(account.Balance, Is.EqualTo(Option<float>.None))


[<Test>]
let ``Cannot update balance of an unopened account`` () =
    let account = BankAccount()
    account.updateBalance 500.0
    Assert.That(account.Balance, Is.EqualTo(Option<float>.None))

    account.openAccount()

    account.updateBalance 500.0
    Assert.That(account.getBalance(), Is.EqualTo(500.0))

[<Test>]
let ``Cannot open an already open account``() =
    let account = BankAccount()
    account.openAccount()
    account.updateBalance 100.0
    account.openAccount()

    Assert.That(account.getBalance, Is.EqualTo(100.0))