(*
    The fraction 49/98 is a curious fraction, as an inexperienced mathematician 
    in attempting to simplify it may incorrectly believe that 49/98 = 4/8, which is correct, 
    is obtained by cancelling the 9s.

    We shall consider fractions like, 30/50 = 3/5, to be trivial examples.

    There are exactly four non-trivial examples of this type of fraction, 
    less than one in value, and containing two digits in the numerator and denominator.

    If the product of these four fractions is given in its lowest common terms, 
    find the value of the denominator.
*)

let getDigits n = [|(n/10.0); (n % 10.0)|] |> Array.map (int >> float)
let rec gcd x y = if y = 0.0 then x else gcd y (x % y)

seq {
    for y in 10.0..99.0 do
    for x in 10.0..99.0 do
        if x < y then yield ( x/y, getDigits x, getDigits y )
}
|> Seq.map(fun (res,x,y ) ->
    if (x.[0] / y.[1] = res && x.[1] = y.[0])
    then (x.[0], y.[1])
    elif (x.[1] / y.[0] = res && x.[0]= y.[1])
    then (x.[1], y.[0])
    else (1.0,1.0)
) 
|> Seq.filter (fun (x, y) -> x <> 1.0 || y <> 1.0) 
|> Seq.reduce (fun ( numP, denomP) ( num, denom) -> numP * num, denomP * denom)
|> (fun (num, denum) -> denum/ (gcd num denum))
