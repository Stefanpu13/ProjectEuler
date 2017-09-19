(*
    We shall say that an n-digit number is pandigital if it makes use 
    of all the digits 1 to n exactly once. 
    For example, 2143 is a 4-digit pandigital and is also prime.

    What is the largest n-digit pandigital prime that exists?
*)

#load "000Common.fsx"
open Common.Utils

// Seems like this algorithm is :
// https://en.wikipedia.org/wiki/Steinhaus%E2%80%93Johnson%E2%80%93Trotter_algorithm
let rec insertAtAllPositions e = function
    | [] -> [[e]]
    | x::xs' as xs -> (e::xs)::[for xs in insertAtAllPositions e xs' -> x::xs]

let rec permute = function
    | [] -> [[]]
    | e::xs -> List.collect (insertAtAllPositions e) (permute xs)

// #time
[7..-1..4] 
|> List.collect(fun x -> 
    [1..x] 
    |> permute 
    |> List.map (Seq.map string >> Seq.reduce (+) >> int)
)
|> List.filter isPrime
|> List.max
