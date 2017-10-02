(*
    By replacing the 1st digit of the 2-digit number *3, 
    it turns out that six of the nine possible values: 
    13, 23, 43, 53, 73, and 83, are all prime.

    By replacing the 3rd and 4th digits of 56**3 with the same digit, 
    this 5-digit number is the first example having seven primes among the ten generated numbers, 
    yielding the family: 56003, 56113, 56333, 56443, 56663, 56773, and 56993. 
    Consequently 56003, being the first member of this family, 
    is the smallest prime with this property.

    Find the smallest prime which, by replacing part of the number (not necessarily adjacent digits)
     with the same digit, is part of an eight prime value family.
*)


(*

    Idea: each num`s digits sum is of the form 3a + b, where a: int; b : 0..2
    By replacing any digits with any other digits we still get number with same
    property -> sum of digits is of the form 3*a + b
    The posssible diff of the sums of replaced digits is:

    In any case at least three numbers will be divisible by 3


    11-> 0*3 + 2    -> 
    22-> 1*3 + 1    ->
    33 -> 2* 3 + 0  ->
    44 -> 2 * 3 + 2 ->
    55 -> 3 * 3 + 1 ->
    66 -> 4 * 3 + 0 ->
    77 = 4 * 3 + 2  ->
    88 -> 5*3 + 1   ->
    99 -> 6 * 3 + 0 ->
    00 -> 0         ->

    111-> 0*3 + 0    -> 
    222-> 1*3 + 0    ->
    333 -> 2* 3 + 0  ->
    444 -> 2 * 3 + 0 ->
    555 -> 3 * 3 + 0 ->
    666 -> 4 * 3 + 0 ->
    777 = 4 * 3 + 0  ->
    888 -> 5*3 + 0   ->
    999 -> 6 * 3 + 0 ->
    000 -> 0         ->

    1. The replacement could never happen on the last digit, as this will result in:
    ***5
    ***2
    ***0
    ....
    And 8 prime group can never be formed 

    2. The number is at least 5 digit 
    3. The number of digits to replace is multiple 3,
    otherwise at least 3 digits are not prime
    (why? because sum(original) - sum(replacement) is will result in 3 num divisible by 3)
*)

let isPrime  = function
| 2L -> true
| n -> Seq.forall (fun d -> n % d <> 0L) (2L::[3L..2L..int64(sqrt(float n))])

(*
    Forming the numbers
    111ab
    11a1b    
    1a11b
    a111b    

    11abc
    1a1bc
    a11bc
    1ab1c
    a1b1c
    ab11c
    ....1 - not possible, as for 0,2,4,6,8 the number is even
    b:[1;3;5;7;9] - otherwise number is even
    a:[0..9], unless a at position '0' - then a: [1..9]

    111abc
    11a1bc
    1a11bc
    a111bc
    11ab1c
    1a1b1c
    a11b1c
    1ab11c
    a1b11c
    ab111c

*)


let choose n k = List.fold (fun p i -> ((n + 1 - i) * p) / i) 1 [1..k]

// largest n where choose n k <= x
let rec largestN n k x =
    if (choose n k ) <= x then n else largestN (n-1) k x
(* 
    Taken from : https://msdn.microsoft.com/en-us/library/aa289166.aspx
    also see: https://stackoverflow.com/questions/127704/algorithm-to-return-all-combinations-of-k-elements-from-n
    accepted answer, section: "Index of Combinations in Lexicographical Order (McCaffrey)"
*)
let combinationAt n k m = 
    let x = ((choose n k) - 1) - m
    let (comb, _, _, _) = 
        [1..k] |> List.fold(fun (comb, x, a, b) _ ->        
            let l = largestN a b x
            ((n - 1 - l)::comb, x - choose l b,l, b - 1)
        ) ([], x, n, k)
    List.rev comb 

(*
    1. Get possible possitions of remaining numbers
    For each possition of remaining number
        2. get positions of replacement nubmers
        3. For each of possible remaining digits
            for each of possible replacemant digits
                4. form number
                5. check is prime        
    1. Form 3 digit replacements
      
    form 5 digit nums
        last place is taken from [1;3;5;7;9]
        generate all combinations of (digitCount - 1)(digitCount-replacementsCount-1) = 
        (5 - 1) (5 - 3 - 1) - this gives position of second remaining number
        remDigits = 
        [0..choose((5 - 1) (5 - 3 - 1)) - 1] |> List.map combinationAt 
    test 5 digit nums
*)



let getPossibleReplacementDigits replacementDigitsCount =  
    [0..9] 
    |> List.map (fun n -> 
        List.init replacementDigitsCount (fun _ -> n)         
    )

let rec permutationWithRepetition perms dCount chCount =
    if dCount > 0 then
        seq {
            for i in 0..chCount - 1 do
                yield! permutationWithRepetition (i::perms) (dCount - 1) chCount
        }
    else (seq perms)        

let digitsCount = 6
let replacementsCount = 3


// 1. Get possible possitions of remaining numbers
let remainingDigitsWithoutLastCount = digitsCount - replacementsCount - 1
let posiblePosCount = choose (digitsCount - 1) remainingDigitsWithoutLastCount
let remainingDigitsWithoutLastPositions = 
    [0..posiblePosCount - 1] 
    |> List.map ( fun pos ->
        combinationAt (digitsCount - 1) remainingDigitsWithoutLastCount pos
        |> List.map ((+) 1)
    )

let remainingNumbersPossiblePos = 
    remainingDigitsWithoutLastPositions
    |> List.map (fun l -> 
        let reversePos = l |> List.map(fun p -> digitsCount - 1 - p)
        List.rev ((digitsCount - 1)::reversePos)
    ) // map each number to digits - 1 - num
    |> List.rev

let formNumber (digitsCount, (remNums, remNumsPos), (rplNums, rplNumsPos)) = 
    let numArr = Array.zeroCreate digitsCount
    for i in 0.. (Seq.length remNums) - 1 do
        numArr.[Seq.item i remNumsPos] <- Seq.item i remNums

    for j in 0.. (Seq.length rplNums)  - 1 do
        numArr.[Seq.item j rplNumsPos] <- Seq.item j rplNums 
    
    ((Array.map string) >> (Array.reduce (+)) >> int64) numArr


let numberIsPartOfPrimeGroup 
    groupCount ((digitsCount, (remD, remDPos), (rplD ,rplDPos)) as input) =
    let num = formNumber input
    if  num > pown 10L (digitsCount - 1)  &&   isPrime num then
        let primeNumsCount = 
            getPossibleReplacementDigits (Seq.length rplD)
            |> List.filter (fun rpl -> 
                formNumber ((digitsCount), (remD, remDPos), (rpl, rplDPos))
                |> isPrime
            )
            |> List.length
        primeNumsCount >= groupCount
    else
        false

let positions = set [0..digitsCount - 1]
let lastPlaceDigits = [1..2..9]
let otherPlacesDigits = [0..9]

let possibleRemainingDigits =         
    permutationWithRepetition [] (digitsCount - replacementsCount) 10 
    |> List.ofSeq 
    |> List.chunkBySize (digitsCount - replacementsCount) 

let possibleReplacementDigits = getPossibleReplacementDigits replacementsCount  

// #time
remainingNumbersPossiblePos
|> Seq.collect(fun remainingDigitPos ->
    //2. get positions of replacement nubmers
    let replacementsDigitsPos = 
        Set.difference positions (set remainingDigitPos)
        |> Set.toList   

    // 3. For each of possible remaining digits 
    possibleReplacementDigits   
    |> Seq.collect (fun replacementDigits ->
        possibleRemainingDigits
        |> Seq.map(fun remainingDigits  -> 
            digitsCount, 
            (remainingDigits, remainingDigitPos),
            (replacementDigits, replacementsDigitsPos)
        )
    )    
)
|> Seq.skipWhile (not << (numberIsPartOfPrimeGroup 8))
|> Seq.head 
|> formNumber

