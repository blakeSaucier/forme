module Forme.Test.StringValidationBuilderTest

open NUnit.Framework
open Forme
open FsUnit

[<Test>]
let ``Basic string restraint`` () =
    let nameRestraint = stringRestraint {
        notEmpty
        notLongerThan 50
    }
    "James S" |> nameRestraint |> should be True