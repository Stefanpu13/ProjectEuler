#load "000Common.fsx"
open Common.Utils

(*
    Let divisors of a number are [2;2;2;3]
    Then:
    2 can be seen between 0 and 3 times -> 4 times
    3 can be seen between 0 and 1 times -> 2 times

    the number of possible divisors is 4 * 2
    See: https://math.stackexchange.com/questions/1638531/number-of-positive-integral-divisors
*)


let getTriangular n = (n * (n+1L)) / 2L 

let product l = l |> Seq.reduce (*)

let getUniqueElemsGroupsLengths l = 
    l 
    |> List.groupBy id 
    |> List.map (fun (k, l) -> List.length l)

let possibleDivisorsCount l = 
    l 
    |> List.map ((+) 1) 
    |> product 



let smallestTriangularWithDivisorsCount primeFactorization minimumDivisorsCount= 
    let rec smallestTriangular n = 
        let possibleFactors = 2L::[3L..2L..int64(sqrt(float n))]
        let currentTriangular = getTriangular n        
        let uniqueElemsGroupsLengths = 
            currentTriangular |> (primeFactorization possibleFactors) |> getUniqueElemsGroupsLengths             
        let divisorsCount = possibleDivisorsCount uniqueElemsGroupsLengths
        if divisorsCount >= minimumDivisorsCount 
        then currentTriangular
        else smallestTriangular (n+1L)

    smallestTriangular 2L 

// #time
// smallestTriangularWithDivisorsCount primeFactorization 500

// ---------------------------------------
(*
    Solution 2
    Some optimizations:    
    Optimization 1:
    A number that obviously has more than 500 divisors is the product of the first 9 primes - N
    But this number is bigger than 2 * 2 * 3 * 3 *...* 19 which also has more thab 500 combinations - 
    [2;2;3;3;5;7;11;13;17;19] |> getUniqueElemsGroupsLengths |> possibleDivisorsCount
    
    So, the searched number minN < N
    All primes of N are <= sqrt(N)
    minN < N => All primes of minN <=sqrt(N)

    So, a table of primes up to sqrt(N) + 1 can be used for prime factorization
*)
let tenthPrime = 29
let firstNinePrimes = getSmallerPrimes tenthPrime
let bigTriangular = product firstNinePrimes 
let primesTable = getSmallerPrimes (int(sqrt(float bigTriangular))) |> Seq.map int64 |> List.ofSeq

let smallestTriangularWithDivisorsCount2 primeFactorization possibleFactors minimumDivisorsCount = 
    let rec smallestTriangular n =         
        let currentTriangular = getTriangular n        
        let uniqueElemsGroupsLengths = 
            currentTriangular |> (primeFactorization possibleFactors ) |> getUniqueElemsGroupsLengths             
        let divisorsCount = possibleDivisorsCount uniqueElemsGroupsLengths
        if divisorsCount >= minimumDivisorsCount 
        then currentTriangular
        else smallestTriangular (n+1L)

    smallestTriangular 2L 

smallestTriangularWithDivisorsCount primeFactorization 500
smallestTriangularWithDivisorsCount2 primeFactorization primesTable 500