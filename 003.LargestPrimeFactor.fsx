(*
    The prime factors of 13195 are 5, 7, 13 and 29.
    What is the largest prime factor of the number 600851475143 ?
*)

#load "000Common.fsx"
open Common.Utils
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



let fastPrimes = getSmallerPrimes (int (sqrt(float 600851475143L)) )
let largest = largestPrimeFactor 600851475143L (fastPrimes |> Seq.map int64)
