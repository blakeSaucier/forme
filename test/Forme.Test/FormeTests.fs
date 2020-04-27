module Forme.Test.FormeTests

open NUnit.Framework
open Forme.StringValidationBuilder
open Forme.IntRestraintBuilder
open Forme.Validator
open Forme.Common
open FsUnit

type Person = 
    { FirstName: string
      LastName: string; 
      Age: int }

let validName = stringRestraint {
    notEmpty
    notLongerThan 100
    notShorterThan 3
}

let noMinors = intRestraint {
    atLeast 19
}

[<Test>]
let ``Basic model validation`` () =
    let james =
        { FirstName = "James"
          LastName = "Saucier"
          Age = 33 }

    let isAllowedToBuyBeer = validateFor<Person> {
        it (fun p -> p.FirstName)   mustBe validName
        it (fun p -> p.LastName)    mustBe validName
        it (fun p -> p.Age)         mustBe noMinors
    }

    match james |> isAllowedToBuyBeer with
    | Ok -> Assert.Pass()
    | ValidationError e -> failwith "Should have passed validation"

[<Test>]
let ``Model validation error message test `` () =
    let blake =
        { FirstName = "Blake"
          LastName = "T"
          Age = 12 }

    let isValid = validateFor<Person> {
        it (fun p -> p.FirstName) mustBe validName
        it (fun p -> p.LastName) mustBe validName
        it (fun p -> p.Age) mustBe noMinors
    }

    let expectedError = "'Age' must be at least 19; 'LastName' must be at least as long as 3"

    match blake |> isValid with
    | Ok -> Assert.Fail()
    | ValidationError e -> e.Message |> should equal expectedError
