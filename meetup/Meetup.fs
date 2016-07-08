module Meetup

open System

type Schedule =
    | First
    | Second
    | Third
    | Fourth
    | Last
    | Teenth

let findDates dayOfWeek year month days =
    days
    |> List.map (fun day -> new DateTime(year, month, day))
    |> List.filter (fun date -> date.DayOfWeek = dayOfWeek)  

let meetupDay dayOfWeek schedule year month = 
    let daysInMonth = [1..DateTime.DaysInMonth(year, month)]
    let find = findDates dayOfWeek year month 
    let findNthIn nth = find >> List.item (nth - 1)
    let findLastIn = find >> List.last

    match schedule with
    | Teenth    -> findNthIn 1 [13..19]
    | First     -> findNthIn 1 daysInMonth
    | Second    -> findNthIn 2 daysInMonth
    | Third     -> findNthIn 3 daysInMonth
    | Fourth    -> findNthIn 4 daysInMonth
    | Last      -> findLastIn  daysInMonth