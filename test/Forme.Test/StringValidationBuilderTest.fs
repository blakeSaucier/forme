module Forme.Test.StringValidationBuilderTest

open NUnit.Framework
open Forme.StringValidationBuilder
open Forme.Common
open FsUnit

[<Test>]
let ``Basic string restraint`` () =
    let nameRestraint = validString {
        notEmpty
        notLongerThan 50
    }

    match "James" |> nameRestraint with
    | Ok -> Assert.Pass()
    | ValidationError e -> failwith "Should have passed validation"

[<Test>]
let ``Basic string validation failure messages`` () =
    let notEmptyRestraint = validString { notEmpty }

    match "" |> notEmptyRestraint with
    | Ok -> failwith "Validation should have failed"
    | ValidationError e -> e.Message |> should equal "must not be empty"

[<Test>]
let ``Length constraint tests`` () =
    let lengthConstraint = validString {
        notLongerThan 50
        notShorterThan 10
    }

    match "Hello" |> lengthConstraint with
    | Ok -> failwith "Should have failed validation"
    | ValidationError e -> Assert.Pass() 
    |> ignore

    match "April is the cruelest month" |> lengthConstraint with
    | Ok -> Assert.Pass
    | ValidationError e -> failwith "Validaiton should have passed"
    |> ignore

    match "Howdy" |> validString { hasLengthOf 5 } with
    | Ok -> Assert.Pass
    | ValidationError e -> failwith "Validation should have passed"
    |> ignore

[<Test>]
let ``String to Int parsing`` () =
    let parsable = validString { parsable_int }
    
    "12"
    |> parsable
    |> function
        | Ok -> Assert.Pass
        | ValidationError _ -> failwith "Validation should have passed"
    |> ignore

    "twelve"
    |> parsable
    |> function
        | Ok -> failwith "Should have failed"
        | ValidationError e -> e.Message |> should equal "'twelve' is not parsable as an Int32"
    |> ignore

[<Test>]
let ``Multiple error messages should be joined`` () =
    let bcPostalCode = validString {
        notLongerThan 6
        notShorterThan 6
        notEmpty
        startsWith "V"
    }

    let expectedError = "must start with 'V'; must not be longer than 6"

    match "Not a postal code" |> bcPostalCode with
    | Ok -> failwith "Validation should have failed"
    | ValidationError e -> e.Message |> should equal expectedError
