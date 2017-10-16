module Statistic

let inline computeCrossCorrelationInRange (func1:^a array) (func2:^a array) startIndex endIndex =
    let rec compute index value =
        if index < endIndex then
            func1.[index] * func2.[index] + value
            |> compute (index + 1)
        else
            value / max (endIndex - startIndex |> float) 0.
    compute 0 0.

let inline computeCrossCorrelation func1 func2 startIndex = 
    let endIndex = min(func1 |> Array.length) (func2 |> Array.length) - 1
    computeCrossCorrelationInRange func1 func2 startIndex endIndex
