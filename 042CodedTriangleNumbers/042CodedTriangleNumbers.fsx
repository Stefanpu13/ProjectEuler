(*
    The nth term of the sequence of triangle numbers is given by, 
    tn = ½n(n+1); so the first ten triangle numbers are:

    1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...

    By converting each letter in a word to a number corresponding 
    to its alphabetical position and adding these values we form a word value. 
    For example, the word value for SKY is 19 + 11 + 25 = 55 = t10. 
    If the word value is a triangle number then we shall call the word a triangle word.

    Using words.txt (right click and 'Save Link/Target As...'), 
    a 16K text file containing nearly two-thousand common English words, 
    how many are triangle words?
*)

open System.IO
open System
let names = 
    (File.ReadAllText "042CodedTriangleNumbers/042CodedTriangleNumbers.txt")
        .Replace("\"", "")        
        .Split([|','|], StringSplitOptions.RemoveEmptyEntries)

let wordWorth = Seq.sumBy(fun ch -> int ch - int 'A' + 1)
let wordsWorth = Array.map wordWorth names     
let neededTriangleNumbers = 
    Seq.initInfinite(fun n -> (n + 1) * (n + 2) / 2)    
    |> Seq.takeWhile (fun n -> n <= Array.max wordsWorth) 

let isTriangleWord word = Seq.contains word neededTriangleNumbers

// #time
wordsWorth
|> Array.filter isTriangleWord
|> Array.length
