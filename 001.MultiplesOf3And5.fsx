(*
    If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
    Find the sum of all the multiples of 3 or 5 below 1000.
*)

let multiplesOf3And5 n = [1..n] |> List.filter (fun i -> i % 3 = 0 || i % 5 = 0)

List.sum  (multiplesOf3And5 999)
