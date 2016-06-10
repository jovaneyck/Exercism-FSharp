module School

let empty = Map.empty

let grade grade school = 
    match school |> Map.tryFind grade with
    | Some students -> students
    | None -> []

let add student g school = 
    let currentStudents = school |> grade g
    
    school 
    |> Map.add g ((student :: currentStudents) |> List.sort)

let roster school = 
    school
    |> Map.toSeq
    |> Seq.sort