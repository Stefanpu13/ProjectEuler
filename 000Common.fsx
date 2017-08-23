namespace Common
open System
open System.Collections
module Utils =

    module BitArrayUtils = 
        let toSeq (bitArr: BitArray) = seq {            
            for i in 0..bitArr.Length - 1 do
                yield bitArr.[i]
        }
                        
    // Algorithm translated from wikipedia

    let getPrimes n =
        let a = BitArray(n, true)
        for i in 2..int (sqrt (float n)) do
            if a.[i] 
            then
                let mutable j = i * i
                while j <= n do
                    a.[j] <- false
                    j <- j + i

        let primesWithZeroAndOne = 
            a 
            |> BitArrayUtils.toSeq
            |> Seq.mapi (fun i b -> (i,b)) 
            |> Seq.filter snd 
            |> Seq.map fst
        Seq.skip 2 primesWithZeroAndOne    

    let primeFactorization n = 
        let primes = getPrimes (int (n/2) )
        let rec getFactors n primeIndex factors = 
            let factor = Seq.item primeIndex primes
            if factor = n 
            then
                factor::factors
            elif n % factor = 0 
            then 
                getFactors (n/factor) (primeIndex) (factor::factors)
            else
                getFactors n (primeIndex + 1) factors
        getFactors n 0 []            
        
    primeFactorization  567
    