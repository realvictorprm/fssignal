module MathUtils
open System.Numerics

module Complex =
    open System.Numerics
    

    /// <summary>
    /// Creates an array of complex numbers from an existing array with the real values. The complex value is per default zero.
    /// </summary>
    /// <param name="data">An array of real values (double precission)</param>
    let inline doubleArrayToComplexArray (data:double[])= Array.map (float >> Complex.op_Implicit) data