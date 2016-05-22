module Gigasecond

open System

let gigasecond birthdate = 
    let aGigaSecond = 10.0 ** 9.0
    let (+) (date : DateTime) seconds = date.AddSeconds(seconds)
    let datePart (dateTime : DateTime) = dateTime.Date

    birthdate + aGigaSecond
    |> datePart