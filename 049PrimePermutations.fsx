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
|> List.groupBy (string >> Seq.sort >> Seq.toArray >> System.String) 
|> List.filter (fun (k, s) -> Seq.length s >= 3 && k <> "1478")
|> List.iter(fun (k, s) -> 
    let arr = s |> Array.ofList   
    let len = Array.length arr
    for i in [0..len - 3] do
        for j in [i+1..len - 1] do
            let diff = arr.[j] - arr.[i]
            let sameDiff e = e - arr.[j] = diff
            if Array.exists sameDiff arr.[j+1..len - 1] then                
                searched <- (arr.[i], diff)
)

[0..2] 
|> List.map ((fun i -> fst searched + (snd searched) * i) >> string)
|> List.reduce (+)
