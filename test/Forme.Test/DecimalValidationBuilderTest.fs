module Forme.Test.DecimalValidationBuilderTest

open NUnit.Framework
open Forme
open FsUnit

[<Test>]
let ``Basic Decimal validation`` () =
    12.000001M
    |> validDecimal { atLeast 10M }
    |> function
        | Ok -> Assert.Pass()
        | ValidationError e -> failwith "Should have passed"

[<Test>]
let ``Decimal non zero validation`` () =
    0.0M
    |> validDecimal { notZero }
    |> function
        | Ok -> failwith "Should have failed validation"
        | ValidationError e -> e.Message |> should equal "must not equal zero"

[<Test>]
let ``Decimal zero precision validation`` () =
    0.00000001M
    |> validDecimal { notZero }
    |> function
        | Ok -> Assert.Pass()
        | _ -> failwith "Should have failed validation"
