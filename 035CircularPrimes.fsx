(*
    The number, 197, is called a circular prime because all rotations of the digits: 
    197, 971, and 719, are themselves prime.

    There are thirteen such primes below 100: 
    2, 3, 5, 7, 11, 13, 17, 31, 37, 71, 73, 79, and 97.

    How many circular primes are there below one million?
*)

#load "000Common.fsx"
open Common.Utils

let rotations str = 
    let numIndexes = 
        seq {
            for i in 0..String.length str - 1 do                         
                for j in 0..String.length str - 1 do 
                    yield (j + i) % 3
            
        }
    numIndexes |> Seq.splitInto (String.length str)

rotations "awe" |> Array.ofSeq

[3..2..1000000] |> List.map (string >> rotations)

[]

0 % 3
1 % 3
2 % 3

1 % 3
2 % 3
3 % 3

2 % 3
3 % 3
4 % 3



for i in  0..2 do
for j in 0..2 do
    printfn "%A" ((j + i) % 3)
