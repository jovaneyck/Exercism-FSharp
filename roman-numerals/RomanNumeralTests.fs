module RomanNumeralTests

open NUnit.Framework
open RomanNumeral
    
[<TestCase(0, ExpectedResult = "")>]
[<TestCase(1, ExpectedResult = "I")>]
[<TestCase(2, ExpectedResult = "II")>]
[<TestCase(3, ExpectedResult = "III")>]
[<TestCase(4, ExpectedResult = "IV")>]
[<TestCase(5, ExpectedResult = "V")>]
[<TestCase(6, ExpectedResult = "VI")>]
[<TestCase(10, ExpectedResult = "X")>]
[<TestCase(9, ExpectedResult = "IX")>]
[<TestCase(27, ExpectedResult = "XXVII")>]
[<TestCase(50, ExpectedResult = "L")>]
[<TestCase(40, ExpectedResult = "XL")>]
[<TestCase(48, ExpectedResult = "XLVIII")>]
[<TestCase(59, ExpectedResult = "LIX")>]
[<TestCase(100, ExpectedResult = "C")>]
[<TestCase(90, ExpectedResult = "XC")>]
[<TestCase(93, ExpectedResult = "XCIII")>]
[<TestCase(141, ExpectedResult = "CXLI")>]
[<TestCase(163, ExpectedResult = "CLXIII")>]
[<TestCase(500, ExpectedResult = "D")>]
[<TestCase(400, ExpectedResult = "CD")>]
[<TestCase(402, ExpectedResult = "CDII")>]
[<TestCase(575, ExpectedResult = "DLXXV")>]
[<TestCase(1000, ExpectedResult = "M")>]
[<TestCase(900, ExpectedResult = "CM")>]
[<TestCase(911, ExpectedResult = "CMXI")>]
[<TestCase(1024, ExpectedResult = "MXXIV")>]
[<TestCase(3000, ExpectedResult = "MMM")>]
let ``Convert roman to arabic numerals`` (arabicNumeral) =
    toRoman arabicNumeral