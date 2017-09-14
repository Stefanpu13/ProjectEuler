(*
    The decimal number, 585 = 10010010012 (binary), is palindromic in both bases.

    Find the sum of all numbers, less than one million, 
    which are palindromic in base 10 and base 2.

    Please note that the palindromic number, 
    in either base, may not include leading zeros.
*)

open System

let checkIsPalindrome (str: string) =     
    [0..str.Length/2] |> List.fold(fun isPalindrome i->
        isPalindrome && str.[i] = str.[str.Length - 1 - i ]    
    ) true    

[1..999999]
|> Seq.map(fun  n -> string n, Convert.ToString(n, 2))
|> Seq.filter (fun (decN, binN) -> checkIsPalindrome decN && checkIsPalindrome binN)
|> Seq.sumBy (fst >> int)

(*
    Faster solution     
    Function isPalindrome(n,base)
     reversed := 0
     k := n
     while k > 0
     reversed := base*reversed + k mod base
     k := k div base
     return (n = reversed) 
*)

let isPalindrome b n = 
    let mutable reversed = 0
    let mutable k = n

    while k > 0 do
        reversed <- b * reversed + k % b
        k <- k / b
    n = reversed 

[1..999999]
|> Seq.filter (fun n -> isPalindrome 10 n && isPalindrome 2 n)
|> Seq.sum

(*
    Much better approach - generate palindroms in both bases

    Function makePalindrome(n,base,oddlength)
    res := n
    if oddlength then n := n div base
    while n > 0
     res := base*res + n mod base
     n := n div base
    return res 
*)

let makePalindrome b isOddLength n = 
    let mutable x = n
    let mutable res = n
    if isOddLength then x <- x / b

    while x > 0 do
        res <- b * res + x % b
        x <- x / b
    res    
// #time
[0..999]
|> List.fold ( fun l x -> (makePalindrome 10 true x)::(makePalindrome 10 false x)::l) []
|> List.filter (isPalindrome 2)
|> List.sum
