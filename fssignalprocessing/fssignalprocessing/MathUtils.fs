module MathUtils
open System
open System.Numerics

module Complex =
    open System.Numerics
    

    /// <summary>
    /// Creates an array of complex numbers from an existing array with the real values. The complex value is per default zero.
    /// </summary>
    /// <param name="data">An array of real values (double precision)</param>
    let inline doubleArrayToComplexArray (data:double[])= Array.map (float >> Complex.op_Implicit) data

type Complex with
    member self.AreSimilar(a:Complex, delta) =
        let areSimilar a b = abs(a - b) < delta
        areSimilar self.Real a.Real && areSimilar self.Imaginary a.Imaginary

    static member RoundTo (numberOfDigits:int) (value:Complex) =
        let real, imaginary = Math.Round(value.Real, numberOfDigits), Math.Round(value.Imaginary, numberOfDigits) in Complex(real, imaginary)