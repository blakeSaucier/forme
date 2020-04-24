module Forme.Test.StringValidationBuilderTest

open NUnit.Framework
open Forme.StringValidationBuilder
open Forme.IntRestraintBuilder
open Forme.Validator
open Forme.Common

[<Test>]
let ``Basic string restraint`` () =
    let nameRestraint = stringRestraint {
        notEmpty
        notLongerThan 50
    }

    match "James" |> nameRestraint with
    | Ok -> Assert.Pass()
    | ValidationError e -> failwith "Should have passed validation"

type Person = { FirstName: string; LastName: string; Age: int }

[<Test>]
let ``Basic model validation`` () =

    let james = {
        FirstName = "James"
        LastName = "Saucier"
        Age = 30
    }

    let firstNameContraint = stringRestraint {
        notEmpty
        notLongerThan 100
        must (fun s -> s = "James") "It must equal 'James'"
    }

    let lastNameContraint = stringRestraint {
        notEmpty
        notLongerThan 20
    }

    let noMinorsOrSeniors = intRestraint {
        atLeast 19
        atMost 70
    }

    let test = validateFor<Person> {
        restrain (fun p -> p.FirstName) firstNameContraint
        restrain (fun p -> p.LastName) lastNameContraint
        restrain (fun p -> p.Age) noMinorsOrSeniors
    }

    match james |> test with
    | Ok -> Assert.Pass()
    | ValidationError e -> failwith "Should have passed validation"