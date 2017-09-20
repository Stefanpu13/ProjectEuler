(*
    It was proposed by Christian Goldbach that every odd composite number 
    can be written as the sum of a prime and twice a square.

    9 = 7 + 2×1^2
    15 = 7 + 2×2^2
    21 = 3 + 2×3^2
    25 = 7 + 2×3^2
    27 = 19 + 2×2^2
    33 = 31 + 2×1^2

    It turns out that the conjecture was false.

    What is the smallest odd composite that cannot be written as the 
    sum of a prime and twice a square?
*)

let isPrime  = function
| 2 -> true
| n -> Seq.forall (fun d -> n % d <> 0) (2::[3..2..int(sqrt(float n))])

let getSquaresToHalf n = 
    Seq.initInfinite (fun n ->  n * n)
    |> Seq.takeWhile (fun sq -> sq <= n / 2)

// #time
Seq.initInfinite (fun n -> 2 *  n + 1)
|> Seq.filter (not << isPrime)
|> Seq.find (fun composite ->
    let squaresToHalf = getSquaresToHalf composite    
    not <| Seq.exists (fun sq -> isPrime ( composite - 2 * sq)) squaresToHalf
)
