(*
    The number 3797 has an interesting property. 
    Being prime itself, it is possible to continuously remove digits from left to right,
     and remain prime at each stage: 3797, 797, 97, and 7. 
     Similarly we can work from right to left: 3797, 379, 37, and 3.

    Find the sum of the only eleven primes that are both truncatable from left to right and right to left.

    NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.
*)

let isPrime n =
    let rec isPrime divisor upperBound = 
        if divisor > upperBound then true
        elif n % divisor = 0 then false
        else isPrime (divisor + 1) upperBound
    
    n > 1 && isPrime 2 (int(sqrt(float n)))

let isTruncatable n =    
    let numDigitCount = (string>>Seq.length) n - 1
    let rec isTruncatable isTr m = 
        match isTr, m with
        | false,  _ -> false
        | _, 1 -> isTr
        | isTr, m -> isTruncatable (isPrime (n / m) && isPrime (n % m)) (m / 10) 

    isTruncatable true (pown 10 numDigitCount) && isPrime n

let rec getTruncatablePrimesSum n primesSum count = 
    match count, isTruncatable n with
    | 11, _ -> primesSum
    | x, false -> getTruncatablePrimesSum (n + 2) primesSum x
    |x, true -> getTruncatablePrimesSum (n + 2) (n + primesSum) (x + 1)      

getTruncatablePrimesSum 23 0 0 

