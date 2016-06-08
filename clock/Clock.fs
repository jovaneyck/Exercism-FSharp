module Clock

type Time = { TotalMinutes : int}
    with
        member this.Hours = this.TotalMinutes / 60
        member this.Minutes = this.TotalMinutes % 60
        member this.Format =
            sprintf "%02d:%02d" this.Hours this.Minutes

let wrap m = 
    let minutesInADay = 1440  
    if m <= 0 then
        minutesInADay + m
    else
        m % minutesInADay

let from h m = { TotalMinutes = h * 60 + m |> wrap }

let plus minutes time = 
    { TotalMinutes = (time.TotalMinutes + minutes) |> wrap }
let minus minutes = plus (-1 * minutes)

type Clock(time) = 
    new(hours) = Clock(from hours 0)
    new(hours, minutes) = Clock(from hours minutes)

    member this.add(m) = Clock(time |> plus m)
    member this.subtract(m) = Clock(time |> minus m)
    
    member private this.hasTime t = time = t

    override this.ToString() = time.Format
    override this.GetHashCode() = time.GetHashCode()
    override this.Equals(other) = 
        match other with
        | :? Clock as c -> c.hasTime time
        | _ -> false
