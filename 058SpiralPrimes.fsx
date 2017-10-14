(*
    Starting with 1 and spiralling anticlockwise in the following way, 
    a square spiral with side length 7 is formed.

    37 36 35 34 33 32 31
    38 17 16 15 14 13 30
    39 18  5  4  3 12 29
    40 19  6  1  2 11 28
    41 20  7  8  9 10 27
    42 21 22 23 24 25 26
    43 44 45 46 47 48 49

    It is interesting to note that the odd squares lie along the bottom right diagonal, 
    but what is more interesting is that 8 out of the 13 numbers 
    lying along both diagonals are prime; that is, a ratio of 8/13 ≈ 62%.

    If one complete new layer is wrapped around the spiral above, 
    a square spiral with side length 9 will be formed. 
    If this process is continued, what is the side length of the square spiral 
    for which the ratio of primes along both diagonals first falls below 10%?
*)

(*
    For each layer:
    the 3 nums (except bottom right diagonal) are
    bottom left: x^2 - (x - 1)
    top left: x^2 - (x-1) * 2
    top right: x^2 - (x-1) * 3
    
    count primes
    if primesCount/totalCount < 10 

*)

// Use fast "isPrime" method
let isPrime n =
    match n with
    | _ when n > 3 && (n % 2 = 0 || n % 3 = 0) -> false
    | _ ->
        let maxDiv = int(sqrt(float n)) + 1
        let rec f d i = 
            if d > maxDiv then 
                true
            else
                if n % d = 0 then 
                    false
                else
                    f (d + i) (6 - i)     
        f 5 2

let getLayerDiagonals sideLength =
    [1..3] 
    |> List.map (fun n -> pown sideLength 2 - (sideLength - 1) * n)

let nextLayerSideLength l = l + 2

Seq.unfold (fun (sideLength, primesCount, totalCount) ->
    let newSideLength = nextLayerSideLength sideLength
    let newPrimesCount = 
        getLayerDiagonals newSideLength
        |> List.filter isPrime
        |> List.length
        |> (+) primesCount

    let newTotalCount = totalCount + 4

    if float newPrimesCount / float newTotalCount < 0.10 then
        None
    else
        Some (newSideLength, (newSideLength, newPrimesCount, newTotalCount))    
) (3, 3, 5)
|> Seq.last
|> nextLayerSideLength


// imperative solution with mutation
let mutable (sideLength, primesCount, totalCount) = (3, 3, 5)

while float primesCount / float totalCount >= 0.1 do
    sideLength <-sideLength + 2

    let newPrimesCount = 
        getLayerDiagonals sideLength
        |> List.filter isPrime
        |> List.length

    primesCount <- primesCount + newPrimesCount
    totalCount <- totalCount + 4

sideLength