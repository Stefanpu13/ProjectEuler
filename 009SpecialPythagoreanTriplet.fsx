(*
    A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,

    a^2 + b^2 = c^2
    For example, 3^2 + 4^2 = 9 + 16 = 25 = 52.

    There exists exactly one Pythagorean triplet for which a + b + c = 1000.
    Find the product abc.
*)

let sq x = x*x

let pr = 
    let mutable prod = 0
    for b in [251..498] do
        for c in [335..499] do
            let a = 1000- (b+c)
            if sq c = sq a + sq b
            then prod  <-  a * b * c
    prod    
        