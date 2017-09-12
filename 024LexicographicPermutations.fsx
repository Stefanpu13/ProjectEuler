
(*
    Idea:
    for n elems: look for which i
    find i:  i * (n-1)! + (n-1)! > searchedCombIndex
    remaning = allBut i
    recursively repeat for remaining elems 
*)

let fact n = List.reduce (*) [1..n]

let getPermutationAt elems permutationIndex = 
    let rec getPermutationAt permutationIndex foundMembers elems = 
        if Array.length elems = 1 then elems.[0] :: foundMembers
        else    
            let remainingElemsPermutationsCount = fact (Array.length elems - 1)
            let nextElemIndex =
                elems 
                |> Array.indexed 
                |> Array.findIndex(fun (i, _) -> 
                    (i + 1) * remainingElemsPermutationsCount >= permutationIndex
                )

            let remainingElems = 
                 Array.concat [
                    Array.take nextElemIndex elems 
                    Array.skip (nextElemIndex + 1) elems
                 ]
            let permsCount = permutationIndex - (nextElemIndex) * remainingElemsPermutationsCount

            getPermutationAt permsCount (elems.[nextElemIndex]::foundMembers) remainingElems    

    getPermutationAt permutationIndex [] elems |> List.rev            

// #time
getPermutationAt [|0..9|] 1000000 
|> List.map string 
|> List.reduce (+)
