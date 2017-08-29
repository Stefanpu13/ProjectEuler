#load "000Common.fsx"
open Common.Utils

// #time    
    (* Python soltion
        primes = [2, 3]
        test = 5

        def is_prime(test, primes):
            for x in primes:
                if x > sqrt(test):
                    return True
                if test % x == 0:
                    return False

        while primes[-1] < 2000000:
            if is_prime(test, primes):
                primes.append(test)
            test += 2

        print(sum(primes[:-1]))

    *)

//
open System.Collections.Generic

let summationOfPrimes (n:int) =
    let primes = List<int64>(n) 
    primes.Add(2L)
    primes.Add(3L)
    let mutable test = 5L
    let num = int64 n

    let isPrime num = 
        let rec isPrime num i =
            if primes.[i] > int64 (sqrt(float num)) then true
            elif num % primes.[i] = 0L then false
            else isPrime num (i+1)
        isPrime num 0    

    while primes.[primes.Count - 1] < num do 
        if isPrime test then primes.Add test
        test <- test + 2L

    (primes |> Seq.sum) - primes.[primes.Count - 1]

summationOfPrimes 2000000
           