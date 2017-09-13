(*
    145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.

    Find the sum of all numbers which are equal to the sum of the factorial of their digits.

    Note: as 1! = 1 and 2! = 2 are not sums they are not include
*)

let fact n = List.fold (*) 1 [1..n]
let digitsFact  = Array.map fact [|0..9|] 

(* 
    one upper bound is 9! * 8 because: 
    (99 999 999) -> 9! * 8 = 2 903 040
    when numOfDigits > 8 then adding 1 digit makes number grow
    at least twice (999... -> 1999...), 
    which from 8 to 9 digits means more than 10 000 000 > 9! 
    9! * 7 = 2540160 which means that some 7-digit number can be curious
*)
let maxPossibleDigits = 8

let sumOfDigitsFactoriels n = 
    let rec sFact  (sum, tens ) n = 
        match tens, n / tens with
        | 1, _ -> sum + digitsFact.[n % 10]
        | _, 0 -> sFact (sum, tens / 10) n
        | x, _ -> sFact (sum + digitsFact.[(n / x) % 10], x / 10) n

    sFact (0, pown 10 (maxPossibleDigits - 1)) n

[3..fact 9 * 8]
|> List.filter (fun x-> x = sumOfDigitsFactoriels x)
|> List.sum
