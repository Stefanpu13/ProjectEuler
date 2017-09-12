(*
    Euler discovered the remarkable quadratic formula:

    n^2+n+41
    It turns out that the formula will produce 40 primes for the consecutive integer values 0≤n≤390≤n≤39. 
    However, when n=40,402+40+41=40(40+1)+41n=40,402+40+41=40(40+1)+41 is divisible by 41,
    and certainly when n=41,412+41+41n=41,412+41+41 is clearly divisible by 41.

    The incredible formula n^2−79n+1601n2−79n+1601 was discovered, 
    which produces 80 primes for the consecutive values 0≤n≤790≤n≤79. 
    The product of the coefficients, −79 and 1601, is −126479.

    Considering quadratics of the form:

    n^2+an+b, where |a|<1000|a|<1000 and |b|≤1000|b|≤1000

    where |n| is the modulus/absolute value of nn
    e.g. |11|=11 and |−4|=4
    Find the product of the coefficients, a and b, 
    for the quadratic expression that produces the maximum number of primes for 
    consecutive values of n, starting with n=0.
*)

let ds = seq {
    for b in [-999..999] do
    for c in [-1000..1000] do        
        if pown b 2 - 4 * c < 0 
        then yield (b,c) 
}    

let isPrime n = 
    let upperBound = int(sqrt(float (n)))
    let rec isPrime divisor = 
        if divisor > upperBound then true
        elif n % divisor = 0 then false
        else isPrime (divisor + 1)
    
    n > 1 && isPrime 2
    
let rec countPrimes n (b,c) = 
    if isPrime (pown n 2 + b*n + c) 
    then countPrimes (n + 1) (b,c)
    else n    

ds |> Seq.maxBy (countPrimes 0) |> (fun (a, b) -> a * b)
