(*
    It can be seen that the number, 125874, and its double, 251748, 
    contain exactly the same digits, but in a different order.

    Find the smallest positive integer, x, 
    such that 2x, 3x, 4x, 5x, and 6x, contain the same digits.
*)

let containSameDigits digits num =     
    Seq.forall (fun d -> Seq.contains d num) digits &&
    Seq.forall (fun d -> Seq.contains d digits) num

let containsSameDigitsTo6 num = 
    let digits = string num    
    List.forall (fun n -> containSameDigits digits (string(n * num))) [2..6]

Seq.initInfinite ((+) 1)
|> Seq.find containsSameDigitsTo6
