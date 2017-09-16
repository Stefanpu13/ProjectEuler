(*
    An irrational decimal fraction is created by concatenating the positive integers:

    0.123456789101112131415161718192021...

    It can be seen that the 12th digit of the fractional part is 1.

    If dn represents the nth digit of the fractional part, 
    find the value of the following expression.

    d1 × d10 × d100 × d1000 × d10000 × d100000 × d1000000
*)

let digitsCount_NumsCount_NumsSize = 
    [|  
        (9, 9, 2.0); 
        (189, 99, 3.0); 
        (2889, 999, 4.0); 
        (38889, 9999, 5.0); 
        (488889, 99999, 6.0); 
        (5888889, 999999, 7.0)
    |]

let previousSumOfDigits digitI = 
     Array.findBack (fun (s, _, _) -> digitI > s) digitsCount_NumsCount_NumsSize
let getIthDigit n i = n / (pown 10 i) % 10

[1..6] 
|> List.map (pown 10)
|> List.fold (fun p digitPosition ->        
    let (digitsCount, numsCount, numSize) = previousSumOfDigits digitPosition
    let (number,digitI) =
        let remainingDigits = float (digitPosition - digitsCount)
        let num = numsCount + (ceil >> int) (remainingDigits / numSize)

        num, (int remainingDigits) % (int numSize)

    (getIthDigit number (int numSize - digitI)) * p
) 1

// Simpler solution taken from(with min changes):
// http://blog.slatner.com/geeky/60-days-of-euler-problem-40.html
let d n =
    let rec generate i total =
        let s = string i
        if n >= total && n <= total + s.Length then
            s.[n - total - 1]
        else
            generate (i + 1) (total + s.Length)

    int(generate 1 0) - int('0')

[0..6]
|> Seq.map (pown 10 >> d)
|> Seq.reduce (*)


// Even simpler solution from(with minor changes): 
// http://theburningmonk.com/2010/09/project-euler-problem-40-solution/
// idea - convert natural numbers seq to a seq of their digits and multiply searched ones 
let naturalNumbers = Seq.unfold (fun x -> Some(x, x+1)) 1
let naturalNumbersDigits = Seq.collect (string >> Seq.map string) naturalNumbers
let getDigitAt i = int(Seq.item (i-1) naturalNumbersDigits)

[0..6]
|> Seq.map (pown 10 >> getDigitAt)
|> Seq.reduce (*)
