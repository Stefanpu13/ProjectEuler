namespace Common
open System.Collections
module Utils =
    let bitArrayToSeq (bitArr: BitArray) = seq {            
        for i in 0..bitArr.Length - 1 do
            yield bitArr.[i]
    }
                        
    // Algorithm translated from wikipedia

    let getSmallerPrimes n =
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
            |> bitArrayToSeq
            |> Seq.mapi (fun i b -> (i,b)) 
            |> Seq.filter snd 
            |> Seq.map fst
            
        Seq.skip 2 primesWithZeroAndOne
    
    let primeFactorization possibleFactors n =             
        let rec getFactors currentFactors possibleFactors n =
            match possibleFactors, n with        
            | [], x  -> if x > 1L then x::currentFactors else currentFactors
            | factor::factors, x ->             
                if n % factor = 0L
                then getFactors (factor::currentFactors) (factor::factors) (n/factor)
                else getFactors currentFactors factors n           
        
        getFactors [] possibleFactors n
