(*
    By starting at the top of the triangle below and moving to adjacent numbers on the row below,
    the maximum total from top to bottom is 23.

    3
    7 4
    2 4 6
    8 5 9 3

    That is, 3 + 7 + 4 + 9 = 23.

    Find the maximum total from top to bottom in triangle.txt 
    (right click and 'Save Link/Target As...'), 
    a 15K text file containing a triangle with one-hundred rows.
*)

open System.IO
open System

let lines = File.ReadLines "067MaximumPathSumII/067MaximumPathSumII.txt"

let split (s:string) = s.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
let nums = 
    lines 
    |> Seq.map (split >> (Seq.map int) >> Seq.toArray) 
    |> Seq.toArray

type BinTree<'a> = 
| Leaf 
| Node of BinTree<'a> * 'a *  BinTree<'a>

let rec buildTree lineNumber index = 
    if lineNumber < 28 //(Array.length nums)
    then 
        let left = buildTree (lineNumber + 1) index
        let right = buildTree (lineNumber + 1) (index + 1)
        Node(left, nums.[lineNumber].[index], right)
    else 
        Leaf
let rec findMaxValuePathFromRoot sum tree =
    match tree with 
    | Leaf -> 0+sum
    | Node (l, a, r) ->
        let leftMax = findMaxValuePathFromRoot a l
        let rightMax = findMaxValuePathFromRoot a r    
        sum + (max leftMax rightMax)


buildTree 0 0 |> findMaxValuePathFromRoot 0