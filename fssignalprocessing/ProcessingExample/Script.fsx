open System
open System.IO
open System.Numerics

// Weitere Informationen zu F# finden Sie unter http://fsharp.org. Im Projekt "F#-Tutorial" finden Sie
// einen Leitfaden zum Programmieren in F#.

let maxStage = 1000

let twiddleFactorTable =
        let constant = 2. * Math.PI / (float maxStage) in 
        [| for i in 0..(maxStage) -> 
            let x = constant * (float i) in Complex(cos x, sin x) |]

printfn "pause"

let a = ""