#load "../008LargestProductInSeries.fsx"
open LargestProductInSeries.M

open System.IO

let getGridFromFile file = 
    File.ReadLines(file) 
    |> Seq.map (fun line -> line.Split([|' '|]) |> Array.map int64) 
    |> Array.ofSeq

let grid = getGridFromFile "011LargestProductInAGrid/grid.txt"

let getSeries startPoint nextPoint (matrix: int64 [] []) = 
    let matrixHeight = matrix.Length
    let matrixWidth = matrix.[0].Length
    let rec getSeries series (x, y) =
        match x, y with
        | widthReached, _ when widthReached >= matrixWidth || widthReached < 0-> series
        | _, heightReached when heightReached >= matrixHeight || heightReached < 0 -> series
        | x, y -> getSeries (matrix.[y].[x]::series) (nextPoint (x, y))

    getSeries [] startPoint 
    |> List.rev 
    |> Array.ofList         

let getHorizontalLineSeries y = getSeries (0, y) (fun (x,y) -> (x+1, y)) grid
let getVerticalLineSeries x = getSeries (x, 0) (fun (x,y) -> (x, y+1)) grid
let getPrimaryDiagonalSeries point = getSeries point (fun (x,y) -> (x+1, y+1)) grid
let getSecondaryDiagonalSeries point = getSeries point (fun (x,y) -> (x-1, y+1)) grid

let getAllHorizontalLineSeries (matrix: int64 [] []) =    
    [|0..matrix.Length - 1|] |> Array.map getHorizontalLineSeries

let getAllVerticalLineSeries (matrix: int64 [] []) = 
    [|0..matrix.[0].Length - 1|] |> Array.map getVerticalLineSeries

let getAllPrimaryDiagonalSeries (matrix: int64 [] []) subseriesLength = 
    let seriesBelowPrimaryDiagonal = 
        [|0..matrix.Length - 1 - subseriesLength|] 
        |> Array.map ((fun y -> (0,y)) >> getPrimaryDiagonalSeries)
    let seriesAbovePrimaryDiagonal = 
        [|0.. matrix.[0].Length - 1- subseriesLength|]
        |> Array.map ((fun x -> (x,0)) >> getPrimaryDiagonalSeries)     
    Array.concat [seriesBelowPrimaryDiagonal; seriesAbovePrimaryDiagonal]

let getAllSecondaryDiagonalSeries (matrix: int64 [] []) subseriesLength = 
    let seriesBelowSecondaryDiagonal = 
        [|0..matrix.Length - 1 - subseriesLength|] 
        |> Array.map ((fun y -> (matrix.[0].Length - 1 ,y)) >> getSecondaryDiagonalSeries)
    let seriesAboveSecondaryDiagonal = 
        [|subseriesLength - 1..matrix.[0].Length - 2|]
        |> Array.map ((fun x -> (x, 0)) >> getSecondaryDiagonalSeries)     
    Array.concat [seriesBelowSecondaryDiagonal; seriesAboveSecondaryDiagonal]

[
    getAllHorizontalLineSeries grid 
    getAllVerticalLineSeries grid
    getAllPrimaryDiagonalSeries grid 4
    getAllSecondaryDiagonalSeries grid 4
] 
|> Seq.collect (Array.map (largestProductInSeries2 4))
|> Seq.max