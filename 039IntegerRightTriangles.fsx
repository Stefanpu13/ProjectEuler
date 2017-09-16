(*
    If p is the perimeter of a right angle triangle with integral length sides, {a,b,c}, 
    there are exactly three solutions for p = 120.

    {20,48,52}, {24,45,51}, {30,40,50}

    For which value of p ≤ 1000, is the number of solutions maximised?
*)

(*
    generate all triples sums <= 1000 
    (See:
        https://en.wikipedia.org/wiki/Coprime_integers#Generating_all_coprime_pairs)
        and 
        https://en.wikipedia.org/wiki/Tree_of_primitive_Pythagorean_triples#Alternative_methods_of_generating_the_tree
    group by sums 
    get sum with max length group (this sum is the perimeter)    
*)

let tripleFromCoprimes (m, n) = (2 * m * n), (m * m + n * n), (m * m - n * n) 
let perimeter (a, b, c) = a + b + c 
let product (a, b, c) n = (a * n, b * n, c * n)

let generateCoprimes maxPerimeter =
    let rec gen l =   
        let (m, n) = List.head l 
        if (tripleFromCoprimes >> perimeter) (m, n) <= maxPerimeter
        then (gen [2 * m - n, m]) @ (gen [2 * m + n, m]) @ (gen [m + 2 * n, n]) @ l
        else []

    gen [(2,1)]

let getCompositeTriples maxPerimeter triple  =
    [1..maxPerimeter] 
    |> Seq.map(product triple)
    |> Seq.takeWhile (fun tr -> perimeter tr <= maxPerimeter)

generateCoprimes 1000 
|> List.map tripleFromCoprimes
|> Seq.collect (getCompositeTriples 1000)
|> Seq.countBy perimeter
|> Seq.maxBy snd
|> fst

