module Forme.Test.StringValidationBuilderTest

open NUnit.Framework
open Forme
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
    ""
    |> validString { notEmpty }
    |> function
        | Ok -> failwith "Validation should have failed"
        | ValidationError e -> e |> should equal [{ Message = "must not be empty" }]

[<Test>]
let ``Length constraint tests`` () =
    let lengthConstraint = validString {
        notLongerThan 50
        notShorterThan 10
    }

    "Hello"
    |> lengthConstraint
    |> function
        | Ok -> failwith "Should have failed validation"
        | ValidationError e -> Assert.Pass()

    "April is the cruelest month"
    |> lengthConstraint
    |> function
        | Ok -> Assert.Pass()
        | ValidationError e -> failwith "Validaiton should have passed"

    "Howdy"
    |> validString { hasLengthOf 5 }
    |> function
        | Ok -> Assert.Pass()
        | ValidationError e -> failwith "Validation should have passed"

[<Test>]
let ``String to Int parsing`` () =
    let parsable = validString { parsable_int }
    
    "12"
    |> parsable
    |> function
        | Ok -> Assert.Pass()
        | ValidationError _ -> failwith "Validation should have passed"

    "twelve"
    |> parsable
    |> function
        | Ok -> failwith "Should have failed"
        | ValidationError e -> e |> should equal [{ Message = "'twelve' is not parsable as an Int32" }]

[<Test>]
let ``Multiple error messages should be joined`` () =
    let bcPostalCode = validString {
        notLongerThan 6
        notShorterThan 6
        notEmpty
        startsWith "V"
    }

    let expectedError = [{ Message = "must start with 'V'"}; { Message = "must not be longer than 6" }]

    "Not a postal code"
    |> bcPostalCode
    |> function
        | Ok -> failwith "Validation should have failed"
        | ValidationError e -> e |> should equal expectedError

[<Test>]
let ``Valid email`` () =
    "a@gmail.com"
    |> validString { email }
    |> function
        | Ok -> Assert.Pass()
        | ValidationError e -> failwith "Should have passed validation"

[<Test>]
let ``Invalid Email`` () =
    "@gmail.com"
    |> validString { email }
    |> function
        | Ok -> failwith "Validation should have failed"
        | ValidationError e -> e |> should equal [{ Message = "'@gmail.com' is not a valid email" }]