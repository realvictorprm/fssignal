open Expecto
open System.Numerics
open MathUtils
open System.Diagnostics


    
let ``FFT Tests`` = 
    test "Radix2-FFT with double precision" {
        let inputData = [| 0.; 1.; 2.; 3. |]
        let referenceResult = [| Complex(1.5, 0.); Complex(-0.5, 0.5); Complex(-0.5, 0.); Complex(-0.5, -0.5); |]
        let result = (SignalProcessing.radix2fftDoublePrecision inputData) |> Array.map(fun complex -> Complex.RoundTo 14 complex)
        for i in 0..3 do Expect.equal result.[i] referenceResult.[i] "The complex numbers must be nearly equal."

        
        let data = Array.init 1000000 (fun _ -> 1.)
        let stopwatch = Stopwatch.StartNew()
           
        for i in 0..20 do 
            stopwatch.Restart()
            let res = Statistic.computeCrossCorrelationInRange data data 0 (data.Length - 1)
            stopwatch.Stop()
            printfn "Computing cross correlation took %A milliseconds. Result value: %A" stopwatch.ElapsedMilliseconds res

    } 


[<EntryPoint>]
let main args =
    runTestsWithArgs defaultConfig args ``FFT Tests`` |> ignore
    System.Console.ReadKey() |> ignore
    0
      