(*
    In England the currency is made up of pound, £, and pence, p, and there are eight coins in general circulation:

    1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).
    It is possible to make £2 in the following way:

    1×£1 + 1×50p + 2×20p + 1×5p + 1×2p + 3×1p
    How many different ways can £2 be made using any number of coins?
*)

(*
    number of possible ways to form:
    2 with 1 -> 1
    5 with 2, 1 ->
        0*2 + 5 with 1 +
        1*2 + 5-2 = 3 with 1 +
        2*2 +  1 with 1
    10 with 5, 2, 1 ->
        0*5 + 10 with 2, 1:
            0 * 2 + 10 with 1
            ................ 
            5 * 2 + 0
        1*5 +             
*)

let rec countCoinSums currentSum count summards = 
    match summards with
    |[] -> 0
    |[x] -> 1
    |s::rest ->            
        [0..currentSum/s] 
        |> List.sumBy(fun x -> countCoinSums (currentSum - x*s) 0 rest)

countCoinSums 200 0 [200; 100;50;20;10;5;2;1] 

// ---------------------------------------
(*
    Optimized solution
*)


let countCoinSums2 s coins =
    let memo  = Array2D.init (s + 1) (Seq.sum coins + 1) (fun _ _ -> 0)
    let rec countCoinSums2 s count coins = 
        match coins with
        |[] -> 0
        |[_] -> 1
        |coin::rest ->                 
            if memo.[s, Seq.sum rest] > 0 
            then memo.[s, Seq.sum rest]
            else 
                memo.[s, Seq.sum rest] <-
                    [0..s/coin] 
                    |> List.sumBy(fun i -> countCoinSums2 (s - coin*i) 0 rest)
                memo.[s, Seq.sum rest]                    
    countCoinSums2 s 0 coins

countCoinSums2 200 [200; 100;50;20;10;5;2;1]

