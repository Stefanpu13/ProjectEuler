(*
    We shall say that an n-digit number is pandigital if it makes use 
    of all the digits 1 to n exactly once. 
    For example, 2143 is a 4-digit pandigital and is also prime.

    What is the largest n-digit pandigital prime that exists?
*)

let isPrime n =
    n = 2 ||
    (
        (n % 2 <> 0) && 
        [3..2..int(sqrt(float n))]|> List.forall (fun d -> n % d <> 0)
    )

let rec distribute e = function
  | [] -> [[e]]
  | x::xs' as xs -> (e::xs)::[for xs in distribute e xs' -> x::xs]

let rec permute = function
  | [] -> [[]]
  | e::xs -> List.collect (distribute e) (permute xs)

// #time
[7..-1..4] 
|> List.collect(fun x -> 
     (permute [1..x]) |> List.map (fun l -> Seq.map string l |> Seq.reduce (+) |> int)
)
|> List.filter isPrime
|> List.max
