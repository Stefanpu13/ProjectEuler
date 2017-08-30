(*
    The following iterative sequence is defined for the set of positive integers:

    n → n/2 (n is even)
    n → 3n + 1 (n is odd)

    Using the rule above and starting with 13, we generate the following sequence:

    13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
    It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms. Although it has not been proved yet (Collatz Problem), it is thought that all starting numbers finish at 1.

    Which starting number, under one million, produces the longest chain?

    NOTE: Once the chain starts the terms are allowed to go above one million.
*)

let (|Even|Odd|) n = 
    if n % 2L = 0L then Even
    else Odd

let getCollatzSequenseCount n = 
    let rec getCollatzSequenseCount count n =
        match n with
        | 1L -> count
        | Even ->
            getCollatzSequenseCount (count + 1) (n/2L)
        | Odd ->
            getCollatzSequenseCount (count + 1) (3L*n + 1L)

    getCollatzSequenseCount 1 n    

// #time

[999999..-1..16] |> List.map int64 
|> List.map( fun n -> (n, getCollatzSequenseCount n)) 
|> List.maxBy (fun (n, seqLength) -> seqLength)

(*
    While building collatz seq, keep number of count for current number
    once the sequence is over, update each number in an array
*)


let collatzSeq (nums:int []) n = 
    let rec collatzSeq countCont elemsOutsideOfRangeCount currentN =        
        if currentN <= int64 (nums.Length - 1) 
        then
            let arrayEl = nums.[int currentN]
            if arrayEl > 0
            then
                countCont elemsOutsideOfRangeCount arrayEl
            else
                let newCountCont = (fun elemsOutsideOfRange c ->                
                    nums.[int currentN]<- c + elemsOutsideOfRangeCount
                    countCont elemsOutsideOfRange (c + 1)
                )
                match currentN with
                | 1L -> newCountCont elemsOutsideOfRangeCount 1
                | Even -> 
                    collatzSeq newCountCont (elemsOutsideOfRangeCount) (currentN / 2L)
                | Odd -> 
                    collatzSeq newCountCont (elemsOutsideOfRangeCount) (3L * currentN + 1L)                 
        else
            match currentN with            
            | Even -> 
                collatzSeq countCont (elemsOutsideOfRangeCount + 1) (currentN / 2L)
            | Odd -> 
                collatzSeq countCont (elemsOutsideOfRangeCount + 1) (3L* currentN + 1L)

    let contF elemsOutsideOfRangeCount c = 
         nums.[int n] <- nums.[int n] + elemsOutsideOfRangeCount

    collatzSeq contF 0 n          

// getCollatzSequenseCount 3L
// getCollatzSequenseCount 10L

(* 
    let x = 999999L
    collatzSeq x
    nums.[int x]
    getCollatzSequenseCount x
*)
// collatzSeq 10L
// collatzSeq 3L
//

let getCollatzMax ()= 
    let nums = Array.init 1000000 (fun i -> 0)
    [999999..-1..16] |> List.map int64 
    |> List.map (collatzSeq nums) |> ignore
    (*
        collatzSeq nums 10L
        collatzSeq nums 3L
        collatzSeq nums 32L
        nums.[0..50]
    *)

    (nums |> Array.mapi (fun i  c -> (i, c)) |> Array.maxBy snd, nums)

// #time
let (max, nums) = getCollatzMax ()

getCollatzSequenseCount 837799L