namespace Common
open System.Collections
module Utils =
    let bitArrayToSeq (bitArr: BitArray) = seq {            
        for i in 0..bitArr.Length - 1 do
            yield bitArr.[i]
    }
                        
    // Algorithm translated from wikipedia
    let mutable counter = 1
    let getSmallerPrimes n =
        let a = BitArray(n, true)
        for i in 2..int (sqrt (float n)) do
            if a.[i] 
            then                
                let mutable j = i * i
                while j < n do
                    counter <- counter + 1
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

    
    // Efficient "isPrime" taken from 
    // http://www.fssnip.net/7E/title/Prime-testing
    let isPrime n =
        match n with
        | _ when n > 3 && (n % 2 = 0 || n % 3 = 0) -> false
        | _ ->
            let maxDiv = int(sqrt(float n)) + 1
            let rec f d i = 
                if d > maxDiv then 
                    true
                else
                    if n % d = 0 then 
                        false
                    else
                        f (d + i) (6 - i)     
            f 5 2