(*
    If the numbers 1 to 5 are written out in words: 
    one, two, three, four, five, then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.

    If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, 
    how many letters would be used?


    NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and forty-two) 
    contains 23 letters and 115 (one hundred and fifteen) contains 20 letters. 
    The use of "and" when writing out numbers is in compliance with British usage.
*)

(*
1
..
10 - ten
..
20 - twenty
21 - twenty one
..
30 - thirty
31 - thirty one
..
100 - one hundred
101 - one hundred and one
..
199 - one hundred and 99

901

1000 - one thousand

Idea - 
number % 100 +

456 / 100
*)

let digitNames = function 
    | 1 -> "one"
    | 2 -> "two"
    | 3 -> "three"
    | 4 -> "four"
    | 5 -> "five"
    | 6 -> "six"
    | 7 -> "seven"
    | 8 -> "eight"
    | 9 -> "nine"
    |_ -> ""
let tenToNineteenNames = function
    | 10 -> "ten"
    | 11 -> "eleven"
    | 12 -> "twelve"
    | 13 -> "thirteen"
    | 14 -> "fourteen"
    | 15 -> "fivteen"
    | 16 -> "sixteen"
    | 17 -> "seventeen"
    | 18 -> "eighteen"
    | 19 -> "nineteen"
    |_ -> ""
let tensNames = function
    | 2 -> "twenty"
    | 3 -> "thirty"
    | 4 -> "forty"
    | 5 -> "fifty"
    | 6 -> "sixty"
    | 7 -> "seventy"
    | 8 -> "eighty"
    | 9 -> "ninety"
    | _ -> "" 
    
let hundred = "hundred"

let rec numberToString (res: string) acountedForHundreds n  = 
    let andStr = if acountedForHundreds then " " else " and " 
    match n with
    | thousand when n = 1000 -> "one thousand"
    | aboveHundred when n >= 100 -> 
        let currentRes = digitNames (aboveHundred / 100)
        numberToString (currentRes+ " " + "hundred") false (aboveHundred % 100)
    | aboveTwenty when n >= 20 ->        
        let currentRes = tensNames (aboveTwenty / 10)
        numberToString (res + andStr + currentRes) true (aboveTwenty % 10)
    | tenToNinteen when n >=10 ->
        let currentRes = tenToNineteenNames tenToNinteen        
        res + andStr + currentRes
    | oneToNine when n >= 1 ->
       let currentRes = digitNames oneToNine
       res + andStr + currentRes
    | zero -> res

numberToString "" true 1000 

let removeWhiteSpaces (s: string) = s.Replace(" ", "") 
let concat l r = sprintf "%s%s" l r

[1..1000] 
|> List.map (numberToString "" true) 
|> List.reduce (+) 
|> removeWhiteSpaces 
|> String.length
