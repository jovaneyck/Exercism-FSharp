module LeapYearTests

open NUnit.Framework
open LeapYear
    
(*
    on every year that is evenly divisible by 4
    except every year that is evenly divisible by 100
    unless the year is also evenly divisible by 400
*)

[<Test>]
let ``Is 1996 a valid leap year`` () = 
    Assert.That(isLeapYear 1996, Is.True)
    
[<Test>]
let ``Is 1997 an invalid leap year`` () = 
    Assert.That(isLeapYear 1997, Is.False)
    
[<Test>]
let ``Is the turn of the 20th century an invalid leap year`` () = 
    Assert.That(isLeapYear 1900, Is.False)
    
[<Test>]
let ``Is the turn of the 25th century a valid leap year`` () = 
    Assert.That(isLeapYear 2400, Is.True)