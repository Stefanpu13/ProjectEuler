(*
    Pentagonal numbers are generated by the formula, Pn=n(3n−1)/2. 
    The first ten pentagonal numbers are:

    1, 5, 12, 22, 35, 51, 70, 92, 117, 145, ...

    It can be seen that P4 + P7 = 22 + 70 = 92 = P8. 
    However, their difference, 70 − 22 = 48, is not pentagonal.

    Find the pair of pentagonal numbers, Pj and Pk, 
    for which their sum and difference are pentagonal
     and D = |Pk − Pj| is minimised; what is the value of D?
*)

let toPentagonNumber x = x *(3*x - 1) / 2

// #time
let isPentagon x = 
    let res  = (1.0 + sqrt (1.0 + 24.0 * float  x)) / 6.0
    res - float (int res) = 0.0

Seq.initInfinite(fun j ->
    [1..j - 1]
    |> Seq.map( fun k -> (j, k))
) 
|> Seq.concat 
|> Seq.find ( fun (k, j) ->
    let jMember = toPentagonNumber j
    let kMember = toPentagonNumber k
    (isPentagon (kMember - jMember) && isPentagon (kMember + jMember))
)
|> (fun (k, j) -> toPentagonNumber (k) - toPentagonNumber (j))
