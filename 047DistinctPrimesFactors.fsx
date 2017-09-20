(*
    The first two consecutive numbers to have two distinct prime factors are:

    14 = 2 × 7
    15 = 3 × 5

    The first three consecutive numbers to have three distinct prime factors are:

    644 = 2² × 7 × 23
    645 = 3 × 5 × 43
    646 = 2 × 17 × 19.

    Find the first four consecutive integers to have four distinct prime factors each. 
    What is the first of these numbers?
*)

let uniquePrimeFactorizationCount n =             
    let rec getFactorsCount currentFactorsCount possibleFactors n =
        match possibleFactors, n with        
        | [], x  -> if x > 1 then (currentFactorsCount + 1) else currentFactorsCount
        | factor::factors, x ->             
            if n % factor = 0 && (n/factor) % factor <> 0 then
                getFactorsCount (currentFactorsCount + 1) (factor::factors) (n/factor)
            elif n % factor = 0 then
                getFactorsCount currentFactorsCount (factor::factors) (n/factor)
            else 
                getFactorsCount currentFactorsCount factors x           
    
    getFactorsCount 0 (2::[3..2..int(sqrt(float(n)))]) n

// #time
[644..140000]
|> List.find (fun n -> 
    let rec fourConcecutiveNumsWithFourFactors n count  = 
        match uniquePrimeFactorizationCount n = 4, count with
        | false, _ -> false
        | true, 3 -> true
        | true, x -> fourConcecutiveNumsWithFourFactors (n + 1) (count + 1)

    fourConcecutiveNumsWithFourFactors n 0 
)
