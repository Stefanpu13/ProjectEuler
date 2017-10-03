(*
    It is not until n = 23, that a value exceeds one-million: 23C10 = 1144066.

    How many, not necessarily distinct, values of  nCr, 
    for 1 ≤ n ≤ 100, are greater than one-million?
*)

    open System

    let choose n k = List.fold (fun p i -> ((n + 1.0 - i) * p) / i) 1.0 [1.0..k]

    ([23.0..100.0]
    |> List.sumBy(fun n ->    
        let halfMembers = (n + 1.0)/2.0
        let greaterThanMillionIndex =  
            ({1.0..halfMembers} 
            |> Seq.skipWhile (fun k -> choose n k < 1000000.0)
            |> Seq.head)
        
        (Math.Floor(halfMembers) - greaterThanMillionIndex) * 2.0 + 
        (Math.Ceiling halfMembers - Math.Floor halfMembers)
    ))
