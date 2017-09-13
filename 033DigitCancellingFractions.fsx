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

seq {
    for y in 10.0..99.0 do
    for x in 10.0..99.0 do
        if x < y
        then yield (x/y, string x,string y) 
}
// |> Array.ofSeq |> Seq.iter (printfn "%A")
|> Seq.filter(fun (res,x,y ) ->
    ((string>>float) x.[0] / (string>>float) y.[1] = res && x.[1] = y.[0]) ||
    ((string>>float) x.[1] / (string>>float) y.[0] = res && x.[0]= y.[1])
)


string 48.0
    ((string>>float) "49".[0] / (string>>float) "98".[1] = 0.5 && "49".[1] = "98".[0]) ||
    ((string>>float) "49".[1] / (string>>float) "98".[0] = 0.5 && "49".[0]= "98".[1])

float '4'
