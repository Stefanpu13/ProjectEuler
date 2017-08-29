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
                // for j in i * i..i..n-1 do a.[j] <- false
                let mutable j = i * i
                while j < n do
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
        let possibleFactors = 2L::[3L..2L..int64(sqrt(float n)) + 1L]
        
        let rec getFactors currentFactors possibleFactors n =
            match possibleFactors, n with        
            | [], x  -> if x > 1L then x::currentFactors else currentFactors
            | factor::factors, x ->             
                if n % factor = 0L
                then getFactors (factor::currentFactors) (factor::factors) (n/factor)
                else getFactors currentFactors factors n           
        
        getFactors [] possibleFactors n    
