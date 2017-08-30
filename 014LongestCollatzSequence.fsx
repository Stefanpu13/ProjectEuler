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

// [999999..-1..16] |> List.map int64 
// |> List.map( fun n -> (n, getCollatzSequenseCount n)) 
// |> List.maxBy (fun (n, seqLength) -> seqLength)

(*
    While building collatz seq, keep number of count for current number
    once the sequence is over, update each number in an array, 
    using a continuation
    T
*)


let collatzSeq (nums:int []) n = 
    let rec collatzSeq countCont currentN =        
        if currentN <= int64 (nums.Length - 1) 
        then
            let arrayEl = nums.[int currentN]
            if arrayEl > 0
            then
                countCont (arrayEl + 1)
            else
                let newCountCont = (fun c ->                
                    nums.[int currentN] <- c 
                    countCont (c + 1)
                )
                match currentN with
                | 1L -> newCountCont 1
                | Even -> 
                    collatzSeq newCountCont (currentN / 2L)
                | Odd -> 
                    collatzSeq newCountCont (3L * currentN + 1L)                 
        else
            let newCountCont = (fun c ->                                
                countCont (c + 1)
            )
            match currentN with            
            | Even -> 
                collatzSeq newCountCont (currentN / 2L)
            | Odd -> 
                collatzSeq newCountCont (3L* currentN + 1L)


    collatzSeq ignore n

let getCollatzMax ()= 
    let nums = Array.init 1000000 (fun i -> 0)
    [999999..-1..16] |> List.map int64 
    |> List.map (collatzSeq nums) |> ignore

    (nums |> Array.mapi (fun i  c -> (i, c)) |> Array.maxBy snd, nums)

// #time
let (max, nums) = getCollatzMax ()

getCollatzSequenseCount 3L