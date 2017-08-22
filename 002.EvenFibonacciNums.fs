
let fib2 = Seq.unfold (fun (n_1,n_2 ) -> Some (n_1+n_2, (n_1+n_2, n_1))) (0, 1)


[0..35] 
    |> List.map( fun i -> Seq.item i fib2) 
    |> (@) [0] 
    |> List.filter (fun el -> el < 4000000 && el % 2 = 0)
    |> List.sum