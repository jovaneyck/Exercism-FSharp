module BinarySearch

let rec binarySearch list value = 
    match list with
    | [] -> None
    | _ ->
        let middleIndex = (list |> List.length) / 2
        let pivot = list.[middleIndex]
        if pivot = value then
            Some middleIndex
        else if value < pivot then
            let left = list.[0..middleIndex - 1]
            binarySearch left value
        else 
            let newStartIndex = middleIndex + 1
            let maxIndex = (List.length list) - 1
            let right = list.[newStartIndex..maxIndex]
            binarySearch right value
            |> Option.map (fun idx -> idx + newStartIndex)