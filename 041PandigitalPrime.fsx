(*
    We shall say that an n-digit number is pandigital if it makes use 
    of all the digits 1 to n exactly once. 
    For example, 2143 is a 4-digit pandigital and is also prime.

    What is the largest n-digit pandigital prime that exists?
*)

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
