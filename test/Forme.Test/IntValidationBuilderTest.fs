module Forme.Test.IntValidationBuilderTest

open NUnit.Framework
open Forme.IntRestraintBuilder
open Forme.Common

[<Test>]
let ``Basic Int validation `` () =
    let ageRestraint = validInt {
        atLeast 19
        atMost 90
    }

    21 
    |> ageRestraint
    |> function
        | Ok -> Assert.Pass()
        | ValidationError e -> failwith "Should have passed validation"