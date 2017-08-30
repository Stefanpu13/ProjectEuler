(*
    Starting in the top left corner of a 2×2 grid, and only being able to move to the right and down,
    there are exactly 6 routes to the bottom right corner.
    How many such routes are there through a 20×20 grid?


    If starting from end point we count possible paths to connected points
    and then do the same for each of the connected points, and so on
                p1 
            p1      p1            
        p1      p2      p1
    p1      p3      p3      p1

    If lines are drawe between 'p's, you get triangle reminding of pascals triangle            


    The task is now: how to build pascals triangle

    but pascal trinagle row elems prepresent combinations of C(n/k) where:
    n - number of row (n in 1..), k - number of elem in row (k in 0..n)

    So task is how to calculate combination elems

    We use: https://en.wikipedia.org/wiki/Binomial_coefficient#Multiplicative_formula
    1 -> 0
    2(size of grid) -> 2 (row whose middle term is number of possible paths for given grid)
    3 -> 4
    4 (1 + 1 * 3)-> 6 ( 2 * 3)

    ....
    20 (1 + 1*19) -> (2 * 19) 
*)


let comb n k = [1m..k] |> List.fold (fun p i -> ((n + 1m - i) * p) / i) 1m
comb 6m 2m