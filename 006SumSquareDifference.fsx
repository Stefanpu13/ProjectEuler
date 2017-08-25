let square (x:int64 ) = x*x
let sumOfSquares n = [1L..n] |> List.sumBy square
let squareOfSum n = [1L..n] |> List.sum |> square

squareOfSum 100L - sumOfSquares 100L