(*
    A perfect number is a number for which the sum of its proper divisors is exactly equal
    to the number. For example, the sum of the proper divisors 
    of 28 would be 1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect number.

    A number n is called deficient if the sum of its proper divisors is less
    than n and it is called abundant if this sum exceeds n.

    As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, 
    the smallest number that can be written as the sum of two abundant numbers is 24. 
    By mathematical analysis, it can be shown that all integers greater than 28123 
    can be written as the sum of two abundant numbers. 
    However, this upper limit cannot be reduced any further by analysis even though
    it is known that the greatest number that cannot be expressed 
    as the sum of two abundant numbers is less than this limit.

    Find the sum of all the positive integers which cannot be written 
    as the sum of two abundant numbers.
*)

let getProperDivisorsSum n = 
    [2..int(sqrt(float n))] 
    |> List.filter (fun div -> n % div = 0 ) 
    |> List.sumBy (fun div-> 
        if div <> n/div
        then div + n/div
        else div
    ) 
    |> (+) 1

let getAbundantNumbers numbers = 
    numbers |> List.filter (fun n -> getProperDivisorsSum n > n)

let markNumberMultiples (a: bool []) num = 
    let rec markNumberMultiples num n =         
        if num * n >= 28123
        then ()
        else 
            a.[num * n] <- false
            markNumberMultiples num (n + 1)
    markNumberMultiples num 2       

let getNonAbundantNumsSum () = 
    let seive = Array.init (28123) (fun x -> true)

    let abundantNumbers = [2..28123] |> getAbundantNumbers

    // mark all multiples of abundant numbers, as they can be presented as sum of two such numbers
    abundantNumbers |> List.iter (markNumberMultiples seive)
    let possibleSollutions = 
        seive         
        |> Array.mapi ( fun i isNonAbundantSum -> (i, isNonAbundantSum))
        |> Array.filter snd
        |> Array.map fst

    let abundantNumbersSet = 
        abundantNumbers  |> Set.ofSeq
    
    possibleSollutions 
    |> Seq.filter (fun n->
        not <| Set.exists (fun an -> Set.contains (n - an) abundantNumbersSet)            
            abundantNumbersSet
    ) 
    |> Seq.sum    


// getNonAbundantNumsSum () 

// -------------------------------------------------------------------------
(*
    Make array of all sums of abundants and sum indexes that are not sum of two abundant nums
*)

let getNonAbundantNumsSum2 () =
    let abundantNums = [2..28123] |> getAbundantNumbers |> Array.ofList
    let allSumsArr = Array.init (28123 * 2 + 1) (fun x -> true)
    
    for i in 0..Array.length abundantNums - 1 do
        for j in  0..i do            
            let sum = abundantNums.[i] + abundantNums.[j]
            allSumsArr.[abundantNums.[i] + abundantNums.[j]] <- false
    
    allSumsArr.[0..28123] 
    |> Array.mapi (fun i b -> i,b)
    |> Array.filter snd
    |> Array.sumBy fst 

// #time
getNonAbundantNumsSum2 ()
