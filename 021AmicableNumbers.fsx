(*
    Let d(n) be defined as the sum of proper divisors of n (numbers less than n which divide evenly into n).
    If d(a) = b and d(b) = a, where a ≠ b, then a and b are an amicable pair and each of a and b are called amicable numbers.

    For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 and 110; therefore d(220) = 284. The proper divisors of 284 are 1, 2, 4, 71 and 142; so d(284) = 220.

    Evaluate the sum of all the amicable numbers under 10000.
*)

// #time
let getProperDivisorsSum n = 
    [2..int(sqrt(float n))] 
    |> List.filter (fun div -> n % div = 0 ) 
    |> List.sumBy (fun div-> div + n/div ) 
    |> (+) 1
    
[2..10000] 
|> List.sumBy (fun n -> 
    let fstAmicable = getProperDivisorsSum n        
    let sndAmicable = getProperDivisorsSum fstAmicable        
    if sndAmicable = n && fstAmicable <> sndAmicable then fstAmicable else 0
)  