(*
    The number, 1406357289, is a 0 to 9 pandigital number because 
    it is made up of each of the digits 0 to 9 in some order, 
    but it also has a rather interesting sub-string divisibility property.

    Let d1 be the 1st digit, d2 be the 2nd digit, and so on. 
    In this way, we note the following:

        d2d3d4=406 is divisible by 2
        d3d4d5=063 is divisible by 3
        d4d5d6=635 is divisible by 5
        d5d6d7=357 is divisible by 7
        d6d7d8=572 is divisible by 11
        d7d8d9=728 is divisible by 13
        d8d9d10=289 is divisible by 17
    Find the sum of all 0 to 9 pandigital numbers with this property.
*)

(*
    make a ceive:
    // ceive 3-digit numbers that are divisible by 17 and have different digits
    for each of those numbers:
        get remaining digits 
        find all digits that, together with num.[0..1] form digit divisible by 13
*)
let digits = set ['0'..'9']
let charsAreUnique (s:string) = set s |> Seq.length = s.Length
let toString x = if x < 10 then "0" + string x else string x

// #time
let uniqueDigitsNums = 
    [0..99] 
    |> Seq.map toString
    |> Seq.filter charsAreUnique

[17L; 13L; 11L; 7L; 5L; 3L;2L; 1L] 
|> List.fold(fun validNums d -> 
    validNums 
    |> Seq.collect(fun x ->
        Set.difference digits (set x)
        |> Seq.map(fun d -> string d + x)
    ) 
    |> Seq.filter (fun (newNum: string) -> int64 newNum.[0..2] % d = 0L)
) uniqueDigitsNums
|> Seq.sumBy int64 
