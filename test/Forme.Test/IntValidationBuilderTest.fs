module IntValidationTest

open Forme
open Expecto

[<Tests>]
let intTests =
    testList "Int Validation Tests" [
        test "Basic int validation" {
            let ageRestraint = validInt {
                atLeast 19
                atMost 90
            }
            
            let res = ageRestraint 21
            Expect.equal res Ok "Should have passed validation"
        }
    ]