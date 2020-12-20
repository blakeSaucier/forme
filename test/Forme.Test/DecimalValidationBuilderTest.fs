module DecimalValidationTests

open Forme
open Expecto

[<Tests>]
let decimalTests =
    testList "Decimal Tests" [
        test "Basic decimal validation" {
            let atLeast10 = validDecimal { atLeast 10M }
            let res = atLeast10 12.000001M
            Expect.equal res Ok "Validation should have passed"
        }
        
        test "Non Zero decimal test" {
            let notZero = validDecimal { notZero }
            let res = notZero 0.0M
            Expect.equal res (ValidationError [{ Message = "must not equal zero" }] ) "Should have failed validation"
        }
        
        test "Non zero decimal test with precision" {
            let notZero = validDecimal { notZero }
            let res = notZero 0.00000001M
            Expect.equal res Ok "Should have passed validation"
        }
    ]