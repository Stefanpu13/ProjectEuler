
(*
    2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
    What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
*)
let product = List.fold (*) 1 

let divideByNums n nums = 
    List.fold(fun res num ->
        if res % num = 0
        then res/num
        else res
    ) n nums

let smallestEvenlyDivisible upper = 
    [1..upper] |> List.fold(fun nums n -> 
        let numbersLessThanHalf = List.filter (fun num -> num <= n/2) nums
        let divided = divideByNums n numbersLessThanHalf
        divided::nums
    ) [] |> product

smallestEvenlyDivisible 20

(*
    Much better solution:
    lcm - least common multiple
    By progressively caclulating the least common multiple of 
    current result and current number, we arrive at least common multiple of all
    numbers    
*)
let rec gcd x y = if y = 0 then x else gcd y (x % y)
 
// (y/gcd) combined in brackets to avoid int overflow
let lcm x y = x * (y / (gcd x y)) 

List.reduce lcm [1..20]
