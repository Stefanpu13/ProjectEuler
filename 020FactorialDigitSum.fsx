(*
    n! means n × (n − 1) × ... × 3 × 2 × 1

    For example, 10! = 10 × 9 × ... × 3 × 2 × 1 = 3628800,
    and the sum of the digits in the number 10! is 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.

    Find the sum of the digits in the number 100!
*)

open System.Numerics

let product = List.reduce (*)
let fact (n: BigInteger) = 
    if n = 0I then 1I else product [1I..n] 

fact 100I |> string |> Seq.sumBy (string >> int)