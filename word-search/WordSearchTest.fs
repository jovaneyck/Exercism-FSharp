﻿module WordSearchTest

open NUnit.Framework

open WordSearch

let puzzle = 
    ["jefblpepre";
     "camdcimgtc";
     "oivokprjsm";
     "pbwasqroua";
     "rixilelhrs";
     "wolcqlirpc";
     "screeaumgr";
     "alxhpburyi";
     "jalaycalmp";
     "clojurermt"]

[<Test>]
let ``Finds start and end index of a substring``() =
    Assert.That(indexes ['a';'b'] [(1,1), 'c';(2,1),'a';(3,1),'b'], Is.EqualTo(Some ((2,1), (3,1))))

[<Test>]
let ``Finds start and end index of a substring when first letter is repeated in larger string``() =
    Assert.That(indexes ['a';'b'] [(1,1), 'a';(2,1),'a';(3,1),'b'], Is.EqualTo(Some ((2,1), (3,1))))

[<Test>]
let ``Finds start and end index of a substring without allowing for intermediate irrelevant letters``() =
    Assert.That(indexes ['a';'b'] [(1,1),'a'; (2,1), 'c'; (3,1),'b'], Is.EqualTo(None))

[<Test>]
let ``Should find horizontal words written left-to-right`` () =
    let actual = find puzzle "clojure"
    Assert.That(actual, Is.EqualTo(Some ((1, 10), (7, 10))))

[<Test>]
let ``Should find horizontal words written right-to-left`` () =
    let actual = find puzzle "elixir"
    Assert.That(actual, Is.EqualTo(Some ((6, 5), (1, 5))))

[<Test>]
let ``Should find vertical words written top-to-bottom`` () =
    let actual = find puzzle "ecmascript"
    Assert.That(actual, Is.EqualTo(Some ((10, 1), (10, 10))))

[<Test>]
let ``Should find vertical words written bottom-to-top`` () =
    let actual = find puzzle "rust"
    Assert.That(actual, Is.EqualTo(Some ((9, 5), (9, 2))))

[<Test>]
let ``Should partition diagonals`` () =
    let input =
        [(1,1),'1';(2,1),'2';(3,1),'3';
        (1,2),'4';(2,2),'5';(3,2),'6';
        (1,3),'7';(2,3),'8';(3,3),'9']
    let expected = 
        [
            [((1, 1), '1'); ((2, 2), '5'); ((3, 3), '9')]; 
            [((2, 1), '2'); ((3, 2), '6')];
            [((3, 1), '3')]; 
            [((1, 2), '4'); ((2, 3), '8')]; 
            [((1, 3), '7')]
        ]
    
    let actual = diagonal input

    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let ``Should find diagonal words written top-left-to-bottom-right`` () =
    let actual = find puzzle "java"
    Assert.That(actual, Is.EqualTo(Some ((1, 1), (4, 4))))

[<Test>]
let ``Should find diagonal words written bottom-right-to-top-left`` () =
    let actual = find puzzle "lua"
    Assert.That(actual, Is.EqualTo(Some ((8, 9), (6, 7))))

[<Test>]
let ``Should partition mirrored diagonals`` () =
    let input =
        [(1,1),'1';(2,1),'2';(3,1),'3';(4,1),'4';
         (1,2),'5';(2,2),'6';(3,2),'7';(4,2),'8';
         (1,3),'9';(2,3),'0';(3,3),'A';(4,3),'B';
         (1,4),'C';(2,4),'D';(3,4),'E';(4,4),'F']
    let expected = 
        [
            [((1, 1), '1')]; 
            [((2, 1), '2'); ((1, 2), '5')];
            [((3, 1), '3'); ((2, 2), '6'); ((1, 3), '9')];
            [((4, 1), '4'); ((3, 2), '7'); ((2, 3), '0'); ((1, 4), 'C')];
            [((4, 2), '8'); ((3, 3), 'A'); ((2, 4), 'D')]; 
            [((4, 3), 'B'); ((3, 4), 'E')];
            [((4, 4), 'F')]
        ]
    
    let actual = mirrorDiagonal input

    Assert.That(actual, Is.EqualTo(expected))


[<Test>]
let ``Should find diagonal words written bottom-left-to-top-right`` () =
    let actual = find puzzle "lisp"
    Assert.That(actual, Is.EqualTo(Some ((3, 6), (6, 3))))

[<Test>]
let ``Should find diagonal upper written top-right-to-bottom-left`` () =
    let actual = find puzzle "ruby"    
    Assert.That(actual, Is.EqualTo(Some ((8, 6), (5, 9))))

[<Test>]
let ``Should not find words that are not in the puzzle`` () =
    let actual = find puzzle "haskell"
    Assert.That(actual, Is.EqualTo(None))

[<Test>]
let ``Should be able to search differently-sized puzzles`` () =
    let differentSizePuzzle =
        ["qwertyuiopz";
         "luamsicrexe";
         "abcdefghijk"]

    let actual = find differentSizePuzzle "exercism"    
    Assert.That(actual, Is.EqualTo(Some ((11, 2), (4, 2))))