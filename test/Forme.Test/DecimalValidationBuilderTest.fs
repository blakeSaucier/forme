module Forme.Test.DecimalValidationBuilderTest

open NUnit.Framework
open Forme

[<Test>]
let ``Basic Decimal validation`` () =
    let validation = validDecimal {
        atLeast 10M
    }

    12.000001M
    |> validation
    |> function
        | Ok -> Assert.Pass()
        | ValidationError e -> failwith "Should have passed"