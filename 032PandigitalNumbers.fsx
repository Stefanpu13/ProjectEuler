(*
    We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly once; 
    for example, the 5-digit number, 15234, is 1 through 5 pandigital.

    The product 7254 is unusual, as the identity, 39 × 186 = 7254, containing multiplicand, multiplier, 
    and product is 1 through 9 pandigital.

    Find the sum of all products whose multiplicand/multiplier/product 
    identity can be written as a 1 through 9 pandigital.

    HINT: Some products can be obtained in more than one way so be sure to only include it once in your sum.
*)

(*
    All digits for  3 nums are 9 
    If multiplicants have more than 5 digits, the product can never have 3 digits

    If multiplicants have less than 5 digits, the product can never have 5 digits
    
    So, multiplicants have exacly 5 digits
*)
let oneToNine2 = set ['1'..'9']

// #time
seq {
    for n in [0..99] do
    for m in [0..9999] do
        if n * m < 10000
        then yield (n * m, string (n * m) + string n + string m)
}
|> Seq.filter (fun (p, s) -> Set.ofSeq s = oneToNine2 )     
|> Seq.distinctBy fst 
|> Seq.sumBy fst
