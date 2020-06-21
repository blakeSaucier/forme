module Forme.Test.FormeTests

open NUnit.Framework
open Forme
open FsUnit

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

let noMinors = validInt {
    atLeast 19
}

[<Test>]
let ``Basic model validation`` () =
    let james =
        { FirstName = "James"
          LastName = "Saucier"
          Age = 33 }

    let isAllowedToBuyBeer = valid<Person> {
        rule (fun p -> p.FirstName)  validName
        rule (fun p -> p.LastName)   validName
        rule (fun p -> p.Age)        noMinors
    }

    james
    |> isAllowedToBuyBeer
    |> function
        | Ok -> Assert.Pass()
        | ValidationError e -> failwith "Should have passed validation"

[<Test>]
let ``Model validation error message test `` () =
    let blake =
        { FirstName = "Blake"
          LastName = "T"
          Age = 12 }

    let isValid = valid<Person> {
        rule (fun p -> p.FirstName) validName
        rule (fun p -> p.LastName)  validName
        rule (fun p -> p.Age)       noMinors
    }

    let expectedError = [{ Message = "'Age' must be at least 19"}; { Message = "'LastName' must be at least as long as 3"}]

    blake
    |> isValid
    |> function
        | Ok -> Assert.Fail()
        | ValidationError e -> e |> should equal expectedError

[<Test>]
let ``Nested models should evaluate object graph`` () =
    
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

    contact
    |> validContact
    |> function
        | Ok -> failwith "Should have failed validation"
        | ValidationError e -> e |> should equal [{ Message = "'Profile' 'LastName' must be at least as long as 2"}]
