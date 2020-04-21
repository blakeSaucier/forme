module Forme.Test.StringValidationBuilderTest

open NUnit.Framework
open Forme
open FsUnit

[<Test>]
let ``Basic string restraint`` () =
    let beAPhoneNumber s = s = "7789952549"

    let nameRestraint = stringRestraint {
        notEmpty
        notLongerThan 50
        must beAPhoneNumber "Should equal 'James'"
    }

    let res = "James" |> nameRestraint
    match res with
    | Ok _ -> Assert.Pass()
    | Error _ -> failwith "Should have passed validation"