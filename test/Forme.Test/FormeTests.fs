module Forme.Test.FormeTests

open NUnit.Framework
open Forme
open FsUnit

type Person = 
    { FirstName: string
      LastName: string; 
      Age: int }

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

    let expectedError = "'Age' must be at least 19; 'LastName' must be at least as long as 3"

    blake
    |> isValid
    |> function
        | Ok -> Assert.Fail()
        | ValidationError e -> e.Message |> should equal expectedError
