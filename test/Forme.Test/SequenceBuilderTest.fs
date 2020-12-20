module SequenceValidationTests

open Forme
open Expecto

[<Tests>]
let sequenceTests =
    testList "sequence validation builder tests" [
        test "Basic sequence validation" {
            let nonEmptyStrings = validSeq<string> {
                eachElementMust ValidString.notEmpty
            }
            
            let res = nonEmptyStrings [ "get"; "up"; "and"; "go" ]
            Expect.equal res Ok "Should pass basic validation"
        }
        
        test "Basic sequence validation with failure" {
            let nonEmptyStrings = validSeq<string> {
                eachElementMust ValidString.notEmpty
            }
            let res = nonEmptyStrings [ "it"; "would"; "all"; ""; "work"; "out" ]
            Expect.equal res (ValidationError [{ Message = "must not be empty" }]) "Should have failed validation"
        }
    ]

