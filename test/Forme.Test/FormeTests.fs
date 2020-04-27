module Forme.Test.FormeTests

open NUnit.Framework
open Forme.StringValidationBuilder
open Forme.IntRestraintBuilder
open Forme.Validator
open Forme.Common

type Person = 
    { FirstName: string
      LastName: string; 
      Age: int }

let james =
    { FirstName = "James"
      LastName = "Saucier"
      Age = 33 }

[<Test>]
let ``Basic model validation`` () =
    let validName = stringRestraint {
        notEmpty
        notLongerThan 100
        notShorterThan 3
    }

    let noMinorsOrSeniors = intRestraint {
        atLeast 19
        atMost 70
    }

    let nightClubValidation = validateFor<Person> {
        it (fun p -> p.FirstName)   mustBe validName
        it (fun p -> p.LastName)    mustBe validName
        it (fun p -> p.Age)         mustBe noMinorsOrSeniors
    }

    match james |> nightClubValidation with
    | Ok -> Assert.Pass()
    | ValidationError e -> failwith "Should have passed validation"