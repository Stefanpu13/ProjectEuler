let sq x = x*x

let pr = 
    let mutable prod = 0
    for b in [251..498] do
        for c in [335..499] do
            let a = 1000- (b+c)
            if sq c = sq a + sq b
            then prod  <-  a * b *c
    prod    
        