module ListOps

let rec foldl f seed = 
    function
    | [] -> seed
    | x :: xs -> foldl f (f seed x) xs

let reverse l = 
    let cons = (fun acc el -> el :: acc)
    foldl cons [] l

let rec foldr f seed l =
    foldl (fun a b -> f b a) seed (reverse l)

let length l = 
    foldr (fun _ s -> s + 1) 0 l

let map f l =
    let mapf el acc = (f el) :: acc
    foldr mapf [] l

let filter f l = 
    let filt el acc = 
        if 
            f el 
        then 
            el :: acc 
        else 
            acc
    foldr filt [] l

let append xs ys = 
    let app el acc = el :: acc
    foldr app ys xs

let concat l = 
    foldr append [] l