module Forme.Test.``String Validation Builder``

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
let ``String to Int parsing should pass`` () =
    let parsable = validString { parsable_int }
    
    "12"
    |> parsable
    |> function
        | Ok -> Assert.Pass()
        | ValidationError _ -> failwith "Validation should have passed"

let ``String to Int parsing should fail`` () =
    let parsableInt = validString { parsable_int }

    "twelve"
    |> parsableInt
    |> function
        | Ok -> failwith "Should have failed"
        | ValidationError e -> e |> should equal [{ Message = "'twele' is not parsable as an Int32" }]

[<Test>]
let ``String to decimal should pass parsing`` () =
    let parsableRestraint = validString { parsable_decimal }
    
    "12.0001"
    |> parsableRestraint
    |> function
        | Ok -> Assert.Pass()
        | ValidationError e -> failwith "Should have passed validation"

[<Test>]
let ``String to decimal should fail parsing`` () =
    let parsableRestraint = validString { parsable_decimal }

    "twelve"
    |> parsableRestraint
    |> function
        | Ok -> failwith "Should have failed validation"
        | ValidationError e -> e |> should equal [{ Message = "'twelve' is not parsable as a decimal" }]

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

[<Test>]
let ``Equality validation`` () =
    let mustEqualTralfamadore = validString { equal "Tralfamadore" }

    "Tralfamadore"
    |> mustEqualTralfamadore
    |> function
        | Ok -> Assert.Pass()
        | ValidationError e -> failwith "Should have passed validation"

[<Test>]
let ``Equality test should fail`` () =
    let mustEqual = validString { equal "Trout" }

    "Kilgore"
    |> mustEqual
    |> function
        | Ok -> failwith "Should have failed validation"
        | ValidationError e -> e |> should equal [{ Message = "Must equal 'Trout'" }]

[<Test>]
let ``Regex test should pass`` () =
    let aReasonableDateFormat = validString {
        regex "(\d{1,4})-(\d{1,2})-(\d{1,2})"
    }
    
    // convenience function
    // let aReasonableDateFormat = ValidString.regex "(\d{1,4})-(\d{1,2})-(\d{1,2})"

    "1989-12-03"
    |> aReasonableDateFormat
    |> function
        | Ok -> Assert.Pass()
        | ValidationError _ -> failwith "Should have passed validation"

[<Test>]
let ``Regex should fail and provide error message`` () =
    let unreasonableDateFormat = validString {
        regex "(\d{1,2})/(\d{1,2})/(\d{1,2})"
    }

    "1990-01-01"
    |> unreasonableDateFormat
    |> function
        | Ok -> failwith "Should have failed validation"
        | ValidationError e -> (List.head e) |> should equal { Message = "'1990-01-01' does not match the regular expression: '(\d{1,2})/(\d{1,2})/(\d{1,2})' "}