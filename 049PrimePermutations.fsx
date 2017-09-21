(*
    The arithmetic sequence, 1487, 4817, 8147, in which each of the terms increases by 3330, 
    is unusual in two ways: (i) each of the three terms are prime, and, 
    (ii) each of the 4-digit numbers are permutations of one another.

    There are no arithmetic sequences made up of three 1-, 2-, or 3-digit primes, 
    exhibiting this property, but there is one other 4-digit increasing sequence.

    What 12-digit number do you form by concatenating the three terms in this sequence?
*)

let isPrime  = function
| 2 -> true
| n -> Seq.forall (fun d -> n % d <> 0) (2::[3..2..int(sqrt(float n))])

let mutable searched = (0, 0) 
// #time
[1000..9999]
|> List.filter isPrime
|> List.groupBy (fun (x) -> 
    (string >> Seq.sort >> Seq.toArray >> System.String) x
)
|> List.filter (fun (k, s) -> Seq.length s >= 3 && k <> "1478")
|> List.iter(fun (k, s) -> 
    let arr = s |> Array.ofList |> Array.indexed    
    for (i, el) in arr.[0..Array.length arr - 3] do
        for (j, otherEl) in arr.[i+1..Array.length arr - 1] do
            let diff = otherEl - el
            if Array.exists (fun (_, e) -> 
                e - otherEl = diff
            ) arr.[j+1..Array.length arr - 1]  then
                // printfn "%A" (el, diff)
                searched <- (el, diff)
                

    // Array.fold(fun st (i, el) ->
    //     st || Array.fold (fun innerSt (j, innerEl)->
    //         let diff = innerEl - el
    //         innerSt ||
    //         Array.exists (fun (_, e) -> 
    //             e - innerEl = diff
    //         ) ar.[j+1..Array.length ar - 1] 
    //     ) false ar.[i+1..Array.length ar - 1]        
    // ) false ar.[0..Array.length ar - 3]    
)

[fst searched; fst searched + snd searched; fst searched + (snd searched) * 2]
|> List.map string
|> List.reduce (+)