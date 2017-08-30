(*
    If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
    Find the sum of all the multiples of 3 or 5 below 1000.
*)

let fib = Seq.unfold (fun (n_1,n_2 ) -> Some (n_1+n_2, (n_1+n_2, n_1))) (0, 1)


[0..35] 
    |> List.map( fun i -> Seq.item i fib) 
    |> (@) [0] 
    |> List.filter (fun el -> el < 4000000 && el % 2 = 0)
    |> List.sum