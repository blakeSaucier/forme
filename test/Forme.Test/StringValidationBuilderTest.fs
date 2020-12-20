module StringValidationTests

open Forme
open Expecto

[<Tests>]
let stringTests =
    testList "String validation tests" [
        testList "Basic string validation" [
            test "Basic string validation" {
                let nameRestraint = validString {
                    notEmpty
                    notLongerThan 50
                }
                
                let res = nameRestraint "James"
                Expect.equal res Ok "Should pass validation"
            }
            
            test "Empty string fails validation" {
                let nameRestraint = validString {
                    notEmpty
                    notLongerThan 50
                }
                
                let res = nameRestraint ""
                Expect.equal res (ValidationError [{ Message = "must not be empty" }]) "Should fail validation"
            }
        ]
        testList "String length constraints" [
            let lengthConstraint = validString {
                notLongerThan 50
                notShorterThan 10
            }
            
            test "String length validation should fail" {
                let res = lengthConstraint "Hello"
                Expect.equal res (ValidationError [{ Message = "must be at least as long as 10" }]) "Should fail validation"
            }
            
            test "String length validation should pass" {
                let res = lengthConstraint "April is the cruelest month"
                Expect.equal res Ok "Should pass validation"
            }
        ]
        testList "String parsing" [
            let parsable = validString { parsable_int }
            test "String to int parsing should work" {
                let res = parsable "12"
                Expect.equal res Ok "Should parse successfully"
            }
            
            test "String to int parsing should fail for non number" {
                let res = parsable "twelve"
                Expect.equal res (ValidationError [{ Message = "'twelve' is not parsable as an Int32" }]) "Should fail validation"
            }
        ]        
        testList "Decimal parsing" [
            let parsableDecimal = validString { parsable_decimal }
            test "String to decimal parsing should work for valid decimal" {
                let res = parsableDecimal "12.0001"
                Expect.equal res Ok "Should parse successfully"
            }
            
            test "String to decimal parsing should fail for invalid decimal" {
                let res = parsableDecimal "twelvepointzero1"
                Expect.equal res (ValidationError [{ Message = "'twelvepointzero1' is not parsable as a decimal" }]) "Should fail validation"
            }
        ]
        testList "Multiple error messages" [
            test "Multiple validation errors" {
                let bcPostalCode = validString {
                    notLongerThan 6
                    notShorterThan 6
                    notEmpty
                    startsWith "V"
                }
                let res = bcPostalCode "MVVVMSDTDX"
                Expect.equal res (ValidationError [
                    { Message = "must start with 'V'" }
                    { Message = "must not be longer than 6" } ]) "Should fail validation with several messages"
            }
        ]
        testList "Email tests" [
            let shouldBeEmail = validString { email }
            test "Should recognize a valid email" {
                let res = shouldBeEmail "a@gmail.com"
                Expect.equal res Ok "Should pass validation"
            }
            
            test "Should recognize an invalid email" {
                let res = shouldBeEmail "@test.com"
                Expect.equal res (ValidationError [{ Message = "'@test.com' is not a valid email" }]) "Should fail validation"
            }
        ]
        testList "String comparision tests" [
            let shouldBeAlien = validString { equal "Tralfamadore" }
            test "Should match equal strings" {
                let res = shouldBeAlien "Tralfamadore"
                Expect.equal res Ok "Should pass validation"
            }
            
            test "Should recognize unequal strings" {
                let res = shouldBeAlien "Kilgore"
                Expect.equal res (ValidationError [{ Message = "Must equal 'Tralfamadore'" }]) "Should fail validation"
            }
        ]
        testList "Regex validation" [
            let aReasonableDateFormat = validString {
                regex "(\d{4})-(\d{1,2})-(\d{1,2})"
            }
            test "Should successfully match regex" {
                let res = aReasonableDateFormat "1989-12-03"
                Expect.equal res Ok "Should pass validation"                
            }
            
            test "Should recognize a string which doesn't match the regex pattern" {
                let res = aReasonableDateFormat "12-03-1989"
                Expect.equal res (ValidationError [{ Message = "'12-03-1989' does not match the regular expression: '(\d{4})-(\d{1,2})-(\d{1,2})' " }]) "Should fail validation"
            }
        ]
    ]