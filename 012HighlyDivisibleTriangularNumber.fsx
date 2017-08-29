#load "000Common.fsx"
open Common.Utils


let getTriangular n = (n * (n+1L)) / 2L 

let product l = l |> List.reduce (*)

let getUniqueElemsGroupsLengths l = 
    l 
    |> List.groupBy id 
    |> List.map (fun (k, l) -> List.length l)

let possibleDivisorsCount l = l |> List.map ((+) 1) |> product 

(*
    Let divisors of a number are [2;2;2;3]
    Then:
    2 can be seen between 0 and 3 times -> 4 times
    3 can be seen between 0 and 1 times -> 2 times

    the number of possible divisors is 4 * 2
    See: https://math.stackexchange.com/questions/1638531/number-of-positive-integral-divisors
*)

let smallestTriangularWith500Divisors () = 
    let rec smallestTriangular n = 
        let currentTriangular = getTriangular n        
        let uniqueElemsGroupsLengths = 
            currentTriangular |> primeFactorization |> getUniqueElemsGroupsLengths 
        let divisorsCount = possibleDivisorsCount uniqueElemsGroupsLengths
        if divisorsCount > 500 
        then currentTriangular
        else smallestTriangular (n+1L)
    smallestTriangular 100L 

// #time
// smallestTriangularWith500Divisors ()
