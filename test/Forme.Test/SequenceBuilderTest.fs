module Forme.Test.``Sequence Validation Builder``

open NUnit.Framework
open Forme

[<Test>]
let ``Basic sequence validation should pass`` () =
    let nonEmptyStrings = validSeq<string> {
        eachElementMust ValidString.notEmpty
    }

    ["once"; "like"; "me"]
    |> nonEmptyStrings
    |> function
        | Ok -> Assert.Pass()
        | ValidationError _ -> failwith "Should have passed"

[<Test>]
let ``Basic sequence validation should not pass`` () =
    let nonEmptyStrings = validSeq<string> {
        eachElementMust ValidString.notEmpty
    }

    ["once"; ""; "again"]
    |> nonEmptyStrings
    |> function
        | Ok -> Assert.Fail "Should have failed"
        | ValidationError _ -> Assert.Pass()

