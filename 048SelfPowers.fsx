(*
    The series, 1^1 + 2^2 + 3^3 + ... + 10^10 = 10405071317.

    Find the last ten digits of the series, 1^1 + 2^2 + 3^3 + ... + 1000^1000.
*)

// let pow x y = 
//     [1I..y] |> List.fold (fun res _ -> res * x) 1I
open System.Numerics

let res = 
    [1I..1000I]
    |> List.sumBy (fun x -> BigInteger.Pow(x, int x))
    |> string

res.[String.length res - 10..]
