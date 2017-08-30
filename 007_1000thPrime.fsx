(*
    By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.
    What is the 10 001st prime number?
*)

let getNthPrime n = 
    let isPrime (primes:int []) num = 
        let upperBorder = int (sqrt(float num))
        let rec isPrime i = 
            if primes.[i] > upperBorder || primes.[i] = 0
            then true
            elif num % primes.[i] = 0 
            then false
            else isPrime (i + 1)

        isPrime 1

    let rec findNextPrime primes currentNum = 
        if isPrime primes currentNum
        then currentNum
        else findNextPrime primes (currentNum + 1)

    let primes = [| for i in 0..n+1 -> 0|]

    [1..n] |> List.fold (fun currentPrime i ->
        let nextPrime = findNextPrime primes (currentPrime + 1)
        primes.[i] <- nextPrime
        nextPrime
    ) 1
        

// solution 2 - functional


let getNthPrime2 n = 
    let isPrime num =
        let upperBound = int (sqrt(float num))
        let  rec isPrime divisor = 
            if divisor > upperBound  
            then true 
            elif  num % divisor = 0
            then false 
            else isPrime (divisor + 2)
        num = 2 || (num % 2 <> 0 && isPrime 3)

    let rec getNthPrime prime = function
    | 0 -> prime
    | x ->
        if isPrime (prime + 1)
        then getNthPrime (prime + 1) (x-1)
        else getNthPrime (prime + 1) (x)  
    getNthPrime 1 n    


// slow isPrime

let isPrime3 n =
    let isPrime = [3..2..int (sqrt(float n))] |> List.exists (fun num -> n % num = 0) |> not
    n = 2 || (n % 2 <> 0 && isPrime)


// #time
getNthPrime 140001
getNthPrime2 130001