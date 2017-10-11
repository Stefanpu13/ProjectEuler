(*
    It is possible to show that the square root of two can be expressed 
    as an infinite continued fraction.

    √ 2 = 1 + 1/(2 + 1/(2 + 1/(2 + ... ))) = 1.414213...

    By expanding this for the first four iterations, we get:

    1 + 1/2 = 3/2 = 1.5
    1 + 1/(2 + 1/2) = 7/5 = 1.4
    1 + 1/(2 + 1/(2 + 1/2)) = 17/12 = 1.41666...
    1 + 1/(2 + 1/(2 + 1/(2 + 1/2))) = 41/29 = 1.41379...

    The next three expansions are 99/70, 239/169, and 577/408,
    but the eighth expansion, 1393/985, is the first example
    where the number of digits in the numerator exceeds 
    the number of digits in the denominator.

    In the first one-thousand expansions, how many fractions contain 
    a numerator with more digits than denominator?
*)

let fractionToString (num, denom) =
    (string num, string denom)

let numeratorDigitsAreMore (num, denom) = 
    Seq.length num > Seq.length denom

let generateFractionPart = 
    Seq.unfold (fun (prev, curr) ->
        Some (2I * curr + prev, (curr, 2I * curr + prev))
    )

Seq.zip (generateFractionPart (1I, 3I)) (generateFractionPart (1I, 2I))
|> Seq.take 999
|> Seq.map fractionToString
|> Seq.filter numeratorDigitsAreMore
|> Seq.length



(* Python solution
    n = 3
    d = 2

    count = 0
    for i in range(1, 1000):
        new_d = n + d
        n += 2*d
        d = new_d
        if len(str(n)) > len(str(d)):
            count += 1
*)


let mutable (length, num, denom) = 0, 3I, 2I
let len = (string >> Seq.length)

for _ in 1..1000 do
    let newN = num + denom
    num <- 2I * denom + num
    denom <- newN
    if len num > len denom then        
        length <- length + 1 

length        

let a: string list = [""]