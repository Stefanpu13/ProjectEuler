(*

    A googol (10^100) is a massive number: one followed by one-hundred zeros; 
    100^100 is almost unimaginably large: one followed by two-hundred zeros. 
    Despite their size, the sum of the digits in each number is only 1.

    Considering natural numbers of the form, a^b, where a, b < 100, 
    what is the maximum digital sum?
*)

open System.Numerics

let sumDigits = string >> Seq.sumBy (string >> int)

seq {
    for a in 1I..99I do
    for b in 1..99 do
        yield (a, b)
}
|> Seq.map (BigInteger.Pow >> sumDigits)
|> Seq.max
