// Weitere Informationen zu F# unter "http://fsharp.org".
// Weitere Hilfe finden Sie im Projekt "F#-Tutorial".
open FsXaml
open FSharp.Plotly

open System
open System.Numerics

#nowarn "9"

open System.Diagnostics
open SignalProcessing
open System.IO
open CefSharp
open CefSharp.Wpf

type MainWindow = XAML<"MainWindow.xaml">

let inline tuple3ofFunc f = (f (), f(), f())

module SampleFunctions =
    
    let inline chirp x = sin(x * x)

let sampleSTFT () =
    let stopwatch = Stopwatch.StartNew()
    let chirp x = sin(x * x)//if x < 10000. then 2. * sin(x) else 1. * cos(x * 0.001)

    let data =
        let size = pown 2 14
        let constant = 2. * Math.PI * 0.0015
        Array.Parallel.init size (fun x -> chirp (double x * constant))
    let triangleFun x = 
        if x >= 0. && x < 0.5 then 2. * x 
        elif x >= 0.5 && x <= 1. then (x - 0.5) * 2.
        else 0.
    //for i in 0..40 do
    //    stopwatch.Restart()
    //    do radix2fftDoublePrecision data
    //    stopwatch.Stop()
    //    printfn "Computing fft took %A milliseconds." stopwatch.ElapsedMilliseconds
    stopwatch.Restart()
    let res = ``STFT using Radix2-FFT`` data triangleFun 500
    stopwatch.Stop()
    printfn "Computing stft took %A milliseconds." stopwatch.ElapsedMilliseconds
    res
    
let show2DPlot matrix = 
    let rownames = ["p3";"p2";"p1"]
    let colnames = ["Tp0";"Tp30";"Tp60";"Tp160"]
                                             
    let colorscaleValue = 
        //StyleParam.ColorScale.Electric
        StyleParam.Colorscale.Custom [(0.0,"#000000");(1.0,"#FF0000")]

    let chart = 
        Chart.Heatmap(matrix,colnames,rownames,Colorscale=colorscaleValue,Showscale=true)
        |> Chart.withSize(1000.,1000.)
        |> Chart.withMarginSize(Left=200.)
    Chart.Show chart

let get3DPlot data =
    data
    |> Chart.Surface
    |> Chart.withSize(1200., 800.)

[<STAThread>]
[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    let data = sampleSTFT () |> Array.Parallel.map (Array.Parallel.map(fun data -> System.Numerics.Complex.Abs data))
    let maxValue = data |> Array.Parallel.map (Array.max) |> Array.max
        // show2DPlot data
    let plot3d = get3DPlot data

    let html = plot3d |> GenericChart.toEmbeddedHTML

    let window = MainWindow()
    // window.MainGrid.Children.Add(browser)
    let tempFile = Path.Combine(Path.GetTempPath(), (Path.GetFileNameWithoutExtension (Path.GetTempFileName()) + ".html"))
    File.WriteAllText(tempFile, html)
    let browser = window.Browser :?> ChromiumWebBrowser
    printfn "path = %A" tempFile
    
    browser.Address <- tempFile
    window.Show()
    /// --------------------------
    /// OLD WPF LOGIC, DEPRECATED
    /// --------------------------
    
    //let image = new WriteableBitmap(data.[0].Length, data.Length, 90., 90., PixelFormats.Rgb24, BitmapPalettes.WebPalette)
    

    //window.ProcessingResult.Source <- image
    //let setPixel x y (color:Color) = 
    //    let pBackBuffer = image.BackBuffer |> NativePtr.ofNativeInt<byte>
    //    let pY = y * image.BackBufferStride
    //    let pX = x * 3
    //    let pos = pX + pY
    //    let ptr = NativePtr.add pBackBuffer pos
    //    NativePtr.set ptr 0 color.R    
    //    NativePtr.set ptr 1 color.G
    //    NativePtr.set ptr 2 color.B
    //let random = Random()
    //let randomColor () = 
    //    let rndm () = 
    //        let res = random.Next 255
    //        Math.Exp(1. / float res) * 255. |> byte

    //    tuple3ofFunc rndm |> Color.FromRgb
    //image.Lock()
    //for x in 0 .. (image.PixelWidth - 1) do 
    //    for y in 0 .. (image.PixelHeight - 1) do
    //        let color = 
    //            let currValue = data.[y].[x] / maxValue
    //            if currValue > 1. then printfn "unallowed value %A" currValue
    //            Color.FromRgb(byte(255. * (exp (-2. + currValue * 2.))), 0uy, 0uy)
    //        setPixel x y color
    //image.AddDirtyRect(Int32Rect(0, 0, image.PixelWidth, image.PixelHeight))
    //image.Unlock()
    //window.ShowDialog() |> ignore
    0 // Integer-Exitcode zurückgeben
