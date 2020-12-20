module FormeTests

open Forme
open Expecto

type Person = 
    { FirstName: string
      LastName: string; 
      Age: int }

type Address =
    { StreetAddress: string
      City: string
      PostalCode: string
      Province: string }

type Contact =
    { Profile: Person
      Address: Address
      IsActive: bool }

let validName = validString {
    notEmpty
    notLongerThan 100
    notShorterThan 3
}

let noMinors = validInt { atLeast 19 }

[<Tests>]
let formeTests =
    testList "Forme Model Tests" [
        testList "Basic model tests" [
            let isAllowedToBuyBeer = valid<Person> {
                rule (fun p -> p.FirstName)  validName
                rule (fun p -> p.LastName)   validName
                rule (fun p -> p.Age)        noMinors
            }        
            
            test "Basic Model Validation" {
                let james =
                    { FirstName = "James"
                      LastName = "Saucier"
                      Age = 33 }

                let res = isAllowedToBuyBeer james
                Expect.equal res Ok "Should pass validation"
            }
            
            test "Basic model validation does not pass" {
                let james =
                    { FirstName = "James"
                      LastName = "B"
                      Age = 18 }
                    
                let res = isAllowedToBuyBeer james
                
                let expectedError = ValidationError [
                    { Message = "'Age' must be at least 19"}
                    { Message = "'LastName' must be at least as long as 3"}
                ]

                Expect.equal res expectedError "Validation should fail"
            }
        ]
        testList "Nested models validation" [
            let validProfile = valid<Person> {
                rule (fun p -> p.LastName)  (ValidString.notShorterThan 2)
            }

            let validPostalCode = validString {
                startsWith "V"
                hasLengthOf 6
            }

            let validAddress = valid<Address> {
                rule (fun a -> a.StreetAddress) ValidString.notEmpty
                rule (fun a -> a.PostalCode) validPostalCode
            }

            let validContact = valid<Contact> {
                rule (fun c -> c.Profile) validProfile
                rule (fun c -> c.Address) validAddress
            }
            
            test "Entire object graph should be validated" {
                let blake =
                    { FirstName = "Blake"
                      LastName = "S"
                      Age = 30 }

                let address =
                    { StreetAddress = "1234 Fake St."
                      City = "Vancouver"
                      Province = "BC"
                      PostalCode = "V1A2B3" }
                let contact =
                    { Profile = blake
                      Address = address
                      IsActive = true }                    
                
                let res = validContact contact
                
                let expectedError = ValidationError [{ Message = "'Profile' 'LastName' must be at least as long as 2" }]
                Expect.equal res expectedError "Should fail validation" 
            }
        ]
    ]