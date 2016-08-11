module PythagoreanTriplet

let triplet x y z = (x, y, z)

let private squared n = pown n 2
let private pythagorean (x,y,z) = squared x + squared y = squared z
let private variants (x,y,z) = [(x,y,z);(x,z,y);(y,z,x)]
let isPythagorean triplet =  
    triplet
    |> variants
    |> Seq.exists pythagorean

let pythagoreanTriplets from til = 
    [for x in [from..til] do
     for y in [x..til] do
     for z in [y..til] do
        yield triplet x y z]
    |> List.filter isPythagorean