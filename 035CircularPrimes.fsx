(*
    The number, 197, is called a circular prime because all rotations of the digits: 
    197, 971, and 719, are themselves prime.

    There are thirteen such primes below 100: 
    2, 3, 5, 7, 11, 13, 17, 31, 37, 71, 73, 79, and 97.

    How many circular primes are there below one million?
*)

#load "000Common.fsx"
open Common.Utils

let rotations str =     
    seq {
        for i in 0..String.length str - 1 do                         
            for j in 0..String.length str - 1 do 
                yield (j + i) % String.length str
    }    
    |> Seq.splitInto (String.length str)
    |> Seq.map(fun rot -> Seq.fold (fun n x -> n + string str.[x]) "" rot) 

// #time
let primesToMillion = getSmallerPrimes 1000000 |> Set.ofSeq
primesToMillion 
|> Seq.map string
|> Seq.filter (fun (n:string) -> 
    ['0';'2';'4';'6';'8';] |> List.forall (string >> (not << n.Contains)) 
)
|> Seq.map rotations
|> Seq.filter (fun rots -> Seq.forall (fun rot -> Set.contains (int rot) primesToMillion) rots)
|> Seq.length
|> (+) 1
