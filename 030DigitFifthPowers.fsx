(*
    Surprisingly there are only three numbers that can be written 
    as the sum of fourth powers of their digits:

    1634 = 14 + 64 + 34 + 44
    8208 = 84 + 24 + 04 + 84
    9474 = 94 + 44 + 74 + 44
    As 1 = 14 is not a sum it is not included.

    The sum of these numbers is 1634 + 8208 + 9474 = 19316.

    Find the sum of all the numbers that can be written 
    as the sum of fifth powers of their digits.
*)

(*
    For 7 digit numbers (> 1 000 000)

    (max sum of fifth power digits)7 * pown 9 5 = 413343 => 
    no 7 digit number and above can be sum of 5th power of it digits

    For 6 digits -> max number is 354294
*)

let sumDigits5thPowers = 
    string >> Seq.sumBy (fun x -> pown ((string>>int) x) 5)

[2..6 * pown 9 5]
|> Seq.filter (fun n -> n = sumDigits5thPowers n)
|> Seq.sum
