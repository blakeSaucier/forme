module Forme.Test.StringValidationBuilderTest

open NUnit.Framework
open Forme.StringValidationBuilder
open Forme.Common
open FsUnit

[<Test>]
let ``Basic string restraint`` () =
    let nameRestraint = stringRestraint {
        notEmpty
        notLongerThan 50
    }

    match "James" |> nameRestraint with
    | Ok -> Assert.Pass()
    | ValidationError e -> failwith "Should have passed validation"

[<Test>]
let ``Basic string validation failure messages`` () =
    let notEmptyRestraint = stringRestraint { notEmpty }

    match "" |> notEmptyRestraint with
    | Ok -> failwith "Validation should have failed"
    | ValidationError e -> e.Message |> should equal "Must not be empty"

[<Test>]
let ``Multiple error messages should be joined`` () =
    let bcPostalCode = stringRestraint {
        notLongerThan 6
        notEmpty
        must (fun s -> s.StartsWith("V")) "Must start with 'V'"
    }

    match "Not a postal code" |> bcPostalCode with
    | Ok -> failwith "Validation should have failed"
    | ValidationError e -> e.Message |> should equal "Must start with 'V'; Must not be longer than 6"
