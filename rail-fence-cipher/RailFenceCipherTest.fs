﻿module RailFenceCipherTest

open NUnit.Framework

open RailFenceCipher

[<Test>]
let ``Encode with two rails`` () =
    let actual = encode 2 "XOXOXOXOXOXOXOXOXO"
    let expected = "XXXXXXXXXOOOOOOOOO"
    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let ``Encode with three rails`` () =
    let actual = encode 3 "WEAREDISCOVEREDFLEEATONCE"
    let expected = "WECRLTEERDSOEEFEAOCAIVDEN"
    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let ``Encode with ending in the middle`` () =
    let actual = encode 4 "EXERCISES"
    let expected = "ESXIEECSR"
    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let ``Decode with three rails`` () =
    let actual = decode 3 "TEITELHDVLSNHDTISEIIEA"
    let expected = "THEDEVILISINTHEDETAILS"
    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let ``Decode with five rails`` () =
    let actual = decode 5 "EIEXMSMESAORIWSCE"
    let expected = "EXERCISMISAWESOME"
    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let ``Decode with six rails`` () =
    let actual = decode 6 "133714114238148966225439541018335470986172518171757571896261"
    let expected = "112358132134558914423337761098715972584418167651094617711286"
    Assert.That(actual, Is.EqualTo(expected))