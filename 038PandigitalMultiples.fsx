(*
    Take the number 192 and multiply it by each of 1, 2, and 3:

    192 × 1 = 192
    192 × 2 = 384
    192 × 3 = 576
    By concatenating each product we get the 1 to 9 pandigital, 192384576. 
    We will call 192384576 the concatenated product of 192 and (1,2,3)

    The same can be achieved by starting with 9 and multiplying by 1, 2, 3, 4, and 5,
    giving the pandigital, 918273645, which is the concatenated product of 9 and (1,2,3,4,5).

    What is the largest 1 to 9 pandigital 9-digit number that can be formed 
    as the concatenated product of an integer with (1,2, ... , n) where n > 1?
*)

(*
    "10000 * 1" + "10000 * 2"  = has 10 digits, so it is no pandigital 
*)
let digits = set ['1'..'9']
let isPandigital str = Seq.length str = 9 && Set.ofSeq str = digits

let rec toPandigital (str:string) multiplier n =         
    match str.Length with
    | l when l < 9 -> toPandigital (str + string (n * multiplier)) (multiplier + 1) n
    | _ -> if isPandigital str then str else ""    

// #time
[2..9999]
|> List.maxBy (toPandigital "" 1)
|> toPandigital "" 1
