module SignalProcessing

module Cache =
    open System
    let private maxN = 20
    let powersOfTwo = Array.init maxN (pown 2)
    let bitReversalOf maxBitCount initialValue =
        let rec compute i value =
            if i < maxBitCount then
                if (powersOfTwo.[maxBitCount - 1 - i] &&& initialValue) > 0 then 
                    compute (i + 1) (value ||| powersOfTwo.[i])
                else
                    compute (i + 1) value
            else
                value
        compute 0 0

    //let bitReversal =
    //    [| for k in 0 .. maxN - 1 ->
    //        let n = powersOfTwo.[k]
    //        [| for i in 0 .. (n - 1) -> bitReversalOf n i|]
    //    |]


open Cache
open System.Numerics
open System
open MathUtils.Complex



let radix2fftDoublePrecision data =
    let N = data |> Array.length
    let nAsDouble = double N
    let maxStage = log10(float N) / log10(2.) |> int
    // printfn "maxstage = %A" maxStage
    let twiddleFactorTable =
        let constant = 2. * Math.PI / (double N) 
        [| for i in 0 .. (powersOfTwo.[maxStage - 1]) do 
            let x = -constant * (double i)
            // printfn "x = %A" x
            yield Complex(cos x, -sin x)|]
    // Create data-array filled with the input data (bit-reversed index-order)
    let mutable complexData = 
        let bitReversalFunc = bitReversalOf maxStage
        [| for i in 0 .. (N - 1) ->
            let bitReversalOfi = bitReversalFunc i in
            data.[bitReversalOfi] / nAsDouble 
            |> Complex.op_Implicit |]
    // Recursive computes the FFT of the data. The Stage value must be initial 1
    for stage in 1 .. maxStage do
        let numberOfPakets = powersOfTwo.[maxStage - stage]  // checked
        let currMaxN = powersOfTwo.[stage]  // checked
        let halfMaxN = powersOfTwo.[stage - 1]  // checked
        let twiddleIndices = Array.Parallel.init halfMaxN (fun k -> k * powersOfTwo.[max(maxStage - stage) 0]) // checked
        for paketNumber in 0..(numberOfPakets - 1) do
            let offset = paketNumber * currMaxN
            let offsetPlusHalfMaxN = halfMaxN + offset
            // Pairwise butterfly computation
            for k in 0..(halfMaxN - 1) do
                let a = complexData.[k + offset]
                let b = complexData.[k + offsetPlusHalfMaxN] * twiddleFactorTable.[twiddleIndices.[k]]
                //printfn "b = %A" b
                //printfn "twiddleIndex %A" twiddleIndices.[k]
                //printfn "twiddle factor %A" twiddleFactorTable.[twiddleIndices.[k]]
                let ra = a + b
                let rb = a - b
                complexData.[k + offset] <- ra
                complexData.[k + offsetPlusHalfMaxN] <- rb

    complexData

let ``STFT using Radix2-FFT`` data windowFunction steps =
    let windowSize = (Array.length data) / steps
    let window = Array.Parallel.init windowSize (fun i -> windowFunction (double i / double windowSize))
    let actualSize = powersOfTwo.[Math.Floor(log10 (double windowSize) / log10 2.) |> int]
    let calcLocalSTFT index =
        let startIndex = windowSize * index
        let weightedData = Array.Parallel.init actualSize (fun i -> if i < windowSize then window.[i] * data.[startIndex + i] else 0.)
        radix2fftDoublePrecision weightedData
    Array.Parallel.init steps calcLocalSTFT