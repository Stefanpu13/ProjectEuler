
(*
    Idea:
    for n elems: look for which i
    find i:  i * (n-1)! + (n-1)! > searchedCombIndex
    remaning = allBut i
    recursively repeat for remaining elems 

    Example:
    find 17th permutation of [0,1,2,3]

    for 0 with index = 0 ->  1 * (4-1)! < 17 => 0 is not searhed number
    for 1 with index = 1 -> 2 * (4-1)! < 17 ....
    
    for 2 with index = 2 -> 3 * (4-1)! > 17 => 2 is searched number and it index is 2

    put 2 in permutation list and get remaining elements in order
    for remaining elementsdo the same
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
            let permsCount = permutationIndex - nextElemIndex * remainingElemsPermutationsCount

            getPermutationAt permsCount (elems.[nextElemIndex]::foundMembers) remainingElems    

    getPermutationAt permutationIndex [] elems |> List.rev            

// #time
getPermutationAt [|0..9|] 1000000 
|> List.map string 
|> List.reduce (+)

getPermutationAt [|0..3|] 17