open System
open System.IO
open System.Numerics

// Weitere Informationen zu F# finden Sie unter http://fsharp.org. Im Projekt "F#-Tutorial" finden Sie
// einen Leitfaden zum Programmieren in F#.



//#region "JsonWriter"

//[<RequireQualifiedAccess>]
//type JsonSaveOptions =
//  /// Format (indent) the JsonValue
//  | None = 0
//  /// Print the JsonValue in one line in a compact way
//  | DisableFormatting = 1           

//[<StructuredFormatDisplay("{_Print}")>]
//type JsonValue =
//  | String of string
//  | Number of decimal
//  | Float of float
//  | Record of properties:(string * JsonValue)[]
//  | Array of elements:JsonValue[]
//  | Boolean of bool
//  | Null 
  
//  member x.WriteTo (w:TextWriter, saveOptions) =

//    let newLine =
//      if saveOptions = JsonSaveOptions.None then
//        fun indentation plus ->
//          w.WriteLine()
//          System.String(' ', indentation + plus) |> w.Write
//      else
//        fun _ _ -> ()

//    let propSep =
//      if saveOptions = JsonSaveOptions.None then "\": "
//      else "\":"

//    let rec serialize indentation = function
//      | Null -> w.Write "null"
//      | Boolean b -> w.Write(if b then "true" else "false")
//      | Number number -> w.Write number
//      | Float number -> w.Write number
//      | String s ->
//          w.Write "\""
//          JsonValue.JsonStringEncodeTo w s
//          w.Write "\""
//      | Record properties ->
//          w.Write "{"                      
//          for i = 0 to properties.Length - 1 do
//            let k,v = properties.[i]
//            if i > 0 then w.Write ","
//            newLine indentation 2            
//            w.Write "\""
//            JsonValue.JsonStringEncodeTo w k
//            w.Write propSep
//            serialize (indentation + 2) v
//          newLine indentation 0
//          w.Write "}"
//      | Array elements ->
//          w.Write "["
//          for i = 0 to elements.Length - 1 do
//            if i > 0 then w.Write ","
//            newLine indentation 2
//            serialize (indentation + 2) elements.[i]
//          if elements.Length > 0 then
//            newLine indentation 0
//          w.Write "]"
  
//    serialize 0 x 

//  static member internal JsonStringEncodeTo (w:TextWriter) (value:string) =
//    if String.IsNullOrEmpty value then ()
//    else 
//      for i = 0 to value.Length - 1 do
//        let c = value.[i]
//        let ci = int c
//        if ci >= 0 && ci <= 7 || ci = 11 || ci >= 14 && ci <= 31 then
//          w.Write("\\u{0:x4}", ci) |> ignore
//        else 
//          match c with
//          | '\b' -> w.Write "\\b"
//          | '\t' -> w.Write "\\t"
//          | '\n' -> w.Write "\\n"
//          | '\f' -> w.Write "\\f"
//          | '\r' -> w.Write "\\r"
//          | '"'  -> w.Write "\\\""
//          | '\\' -> w.Write "\\\\"
//          | _    -> w.Write c

//#endregion


//[<Literal>]
//let location = @"E:\Development\speech_recognition\fssignalprocessing\fssignalprocessing\fssignalprocessing"

//[<Literal>]
//let cacheFileName = "SignalProcessingPrecalculations.json"

//let outputPath = Path.Combine(location, cacheFileName)

//// Skriptcode für die Bibliothek hier definieren
//type CacheFileStructure = {
//    PowersOfTwo : decimal[]
//} with
//    member self.ToJsonRepresentation () =
//        JsonValue.Record 
//            [| "PowersOfTwo", ( self.PowersOfTwo |> Array.map JsonValue.Number |> JsonValue.Array)
//               "Description", JsonValue.String "Cache file containing precalculated twittle factors and powers of two" |]

//let powersOfTwo = Array.init 30 (pown 2 >> decimal)

//let cache = { PowersOfTwo = powersOfTwo }

//let writer = StringWriter()
//cache.ToJsonRepresentation().WriteTo(writer, JsonSaveOptions.None)
//writer.Flush()
//File.WriteAllText(outputPath, writer.ToString())



let inline doubleArrayToComplexArray (data:double[])= Array.map (float >> Complex.op_Implicit) data




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



let radix2fftRealToComplex data =
    let N = data |> Array.length
    let nAsDouble = double N
    let maxStage = log10(float N) / log10(2.) |> int
    printfn "maxstage = %A" maxStage
    let twiddleFactorTable =
        let constant = 2. * Math.PI / (double N) 
        [| for i in 0 .. maxStage do 
            let x = -constant * (double i)
            printfn "x = %A" x
            yield Complex(cos x, sin x)|]
    let mutable complexData = doubleArrayToComplexArray (data |> Array.map(fun v -> v / nAsDouble))
    do // Reverses data
        let bitReversalFunc = bitReversalOf maxStage
        for i in 0 .. (powersOfTwo.[maxStage - 1] - 1) do
            let bitReversal =  bitReversalFunc i
            let cache = complexData.[i]
            complexData.[i] <- complexData.[bitReversal] 
            complexData.[bitReversal] <- cache
    // Recursive computes the FFT of the data. The Stage value must be initial 1
    for stage in 1 .. maxStage do
        let numberOfPakets = powersOfTwo.[maxStage - stage]  // checked
        let currMaxN = powersOfTwo.[stage]  // checked
        let halfMaxN = powersOfTwo.[stage - 1]  // checked
        let twiddleIndices = Array.init halfMaxN (fun k -> k * numberOfPakets) // checked
        for paketNumber in 0..(numberOfPakets - 1) do
            let offset = paketNumber * currMaxN
            let offsetPlusHalfMaxN = halfMaxN + offset
            // Pairwise butterfly computation
            for k in 0..(halfMaxN - 1) do
                let a = complexData.[k + offset]
                let b = complexData.[k + offsetPlusHalfMaxN] * twiddleFactorTable.[twiddleIndices.[k]]
                printfn "b = %A" b
                printfn "twiddleIndex %A" twiddleIndices.[k]
                printfn "twiddle factor %A" twiddleFactorTable.[twiddleIndices.[k]]
                let ra = a + b
                let rb = a - b
                complexData.[k + offset] <- ra
                complexData.[k + offsetPlusHalfMaxN] <- rb

    complexData