
let largestPrimeFactor n primes = 
    let rec divideByPrime n res =
        if res % n <> int64 0 then res
        else divideByPrime n (res/n) 
        
    let rec largestPrimeFactor currentDivisionResult previousPrime currentPrimeIndex =        
        if currentDivisionResult = int64 1
        then previousPrime
        else
            let currentPrime = Seq.item currentPrimeIndex primes 
            let res = divideByPrime currentPrime currentDivisionResult
            largestPrimeFactor res currentPrime (currentPrimeIndex + 1)

    largestPrimeFactor n (int64 2) 0         

// Algorithm translated from wikipedia
let getPrimes (n:int) =
    let a = [|0..n|] |> Array.map (fun i -> true)
    for i in 2..int (sqrt (float n)) do
        if a.[i] 
        then
            let mutable j = i * i
            while j <= n do
                a.[j] <- false
                j <- j + i

    let primesWithZeroAndone = 
        a 
        |> Array.mapi (fun i b -> (i,b)) 
        |> Array.filter snd 
        |> Array.map fst
    primesWithZeroAndone.[2..]    

let fastPrimes = getPrimes (int (sqrt(float 600851475143L)) )
let largest = largestPrimeFactor 600851475143L (fastPrimes |> Seq.map int64)
