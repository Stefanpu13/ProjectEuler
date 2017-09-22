(*
    The prime 41, can be written as the sum of six consecutive primes:

    41 = 2 + 3 + 5 + 7 + 11 + 13
    This is the longest sum of consecutive primes that adds to a prime below one-hundred.

    The longest sum of consecutive primes below one-thousand that adds to a prime, 
    contains 21 terms, and is equal to 953.

    Which prime, below one-million, can be written as the sum of the most consecutive primes?
*)

#load "000Common.fsx"
open Common.Utils


// let isPrime  = function
// | 2L -> true
// | n -> Seq.forall (fun d -> n % d <> 0L) (2L::[3L..2L..int64(sqrt(float n))])


// // #time
// let l = 
//     getSmallerPrimes 1000000
//     |> Seq.map int64
//     |> Array.ofSeq




// let getPrimesSumCount maxCount i num (l: int64 [])= 
//     let rec getPrimesSumCount sI eI sum =
//         match sI,eI, sum with
//         // | 0, _,_ -> 0
//         | _, e , _ when e > i - 1 || e < 0 -> 0
//         | s, e, _ when e-s <= maxCount -> 0 
//         | s, e, sm when sm < num ->
//             let newSum = sum - l.[s]  + l.[e + 1]
//             // let newSum = Array.sum (l.[s+1..e+1])
//             getPrimesSumCount (s + 1) (e + 1) newSum
//         | s, e, bg when bg > num ->
//             let newSum = sum - l.[e]
//             let newSum = Array.sum (l.[s..e-1])
//             getPrimesSumCount s (e - 1) newSum
//         | s, e, sum when sum = num ->  e - s      
//         | _ -> -10 

//     getPrimesSumCount 0 (i - 1) (Array.sum l)


// // #time
// l.[0..200]
// |> Array.indexed
// |> Array.fold(fun (maxCount, maxEl) (i, el) ->
//     let smallerPrimes = l.[0..i - 1]
//     let summardsCount = getPrimesSumCount maxCount i el smallerPrimes
//     if summardsCount > maxCount then
//         (summardsCount, el)
//     else 
//         (maxCount, maxEl)   
// ) (21, 953L)


let l = 
    getSmallerPrimes 1000000 
    |> Array.ofSeq

let possibleSumPrimesLength =
    let mutable sumOfPrimes = 0
    Seq.takeWhile(fun pr -> 
        sumOfPrimes <- pr + sumOfPrimes 
        sumOfPrimes < 1000000
    ) l
    |> Seq.length

let startingWithFirstPrime i =     
    (possibleSumPrimesLength - i), Array.sum l.[i..possibleSumPrimesLength - 1]   

let endingWithLastPrime i =  i, Array.sum l.[0..i]

let maxSummardsCountPrime primeGenerator = 
    [0..possibleSumPrimesLength] 
    |>List.map primeGenerator
    |> List.filter (snd >> isPrime)
    |> List.maxBy fst

max 
    (maxSummardsCountPrime startingWithFirstPrime) 
    (maxSummardsCountPrime endingWithLastPrime)


(*
    find all primes whose sum is < 1 000 000 and their sum.

    substract primes from sum , starting from the first prime, 
    until new prime is formed -> remember it and summards count

    ssubstract primes from sum , starting from the last prime, 
    until new prime is formed -> remember it and summards count

    compare summards count of two found primes

    Why this works?
    replacing any smaller prime with a bigger prime will give sum > 1 000 000

*)