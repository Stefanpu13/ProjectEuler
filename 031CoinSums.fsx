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
let coins = [|1;2;5;10;20;50;100;200|]
let memo  = Array2D.init 9 (Seq.sum coins + 1) (fun x y-> 0)

let rec countCoinSums2 currentSum count summards = 
    match summards with
    |[] -> 0
    |[x] -> 1
    |s::rest ->            
        if memo.[currentSum/s, Seq.sum rest] > 0 
        then memo.[currentSum/s, Seq.sum rest]
        else 
            memo.[currentSum/s, Seq.sum rest] <-
                [0..currentSum/s] 
                |> List.sumBy(fun x -> countCoinSums2 (currentSum - x*s) 0 rest)
            memo.[currentSum/s, Seq.sum rest]                    

countCoinSums2 5 0 [5;2;1]

countCoinSums2 10 0 [10;5;2;1]
countCoinSums2 20 0 [20;10;5;2;1] 
countCoinSums2 200 0 [200; 100;50;20;10;5;2;1] 

