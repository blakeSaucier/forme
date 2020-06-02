module Forme.Test.IntValidationBuilderTest

open NUnit.Framework
open Forme.IntRestraintBuilder
open Forme.Common

[<Test>]
let ``Basic Int restraint `` () =
    let ageRestraint = validInt {
        atLeast 19
        atMost 90
    }

    match 21 |> ageRestraint with
    | Ok -> Assert.Pass()
    | ValidationError e -> failwith "Should have passed validation"