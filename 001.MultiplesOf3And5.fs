let multiplesOf3And5 n = [1..n] |> List.filter (fun i -> i % 3 = 0 || i % 5 = 0)

List.sum  (multiplesOf3And5 999)
