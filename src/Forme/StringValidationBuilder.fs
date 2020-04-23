module Forme.StringValidationBuilder

open System
open Common

type StringRestraint = string -> ValidationResult

type StringRestraints = { Restraints: StringRestraint list }

type StringValidationBuilder() =
    member __.Yield _ = { Restraints = List.Empty }
    member __.Run(validation: StringRestraints) (str: string) =
        let evaluated = validation.Restraints |> List.map (fun v -> v str)
        let failures = evaluated |> List.choose (fun v ->
            match v with
            | ValidationError err -> Some err
            | _ -> None)
        match failures with
            | [] -> Ok
            | _ -> ValidationError "Failed"

    /// String cannot be null or whitespace
    [<CustomOperation "notEmpty">]
    member __.NotEmpty(validators: StringRestraints) =
        let mustNotBeEmpty s =
            match s |> String.IsNullOrWhiteSpace with
            | true -> ValidationError "Must not be empty"
            | false -> Ok
        { validators with Restraints = mustNotBeEmpty :: validators.Restraints }

    /// The string's length must be less than or equal to the specified length
    [<CustomOperation "notLongerThan">]
    member __.NotLongerThan(validators: StringRestraints, length) =
        let notLongerThanLength length str = 
            match str |> String.length <= length with
            | true -> Ok
            | false -> ValidationError "must not be longer than %i"
        { validators with Restraints = notLongerThanLength length :: validators.Restraints }

    /// Describe a custom string validation
    [<CustomOperation "must">]
    member __.Must(validators: StringRestraints, (restraint: string -> bool), message) =
        let customRestraint str  =
            match restraint str with
            | true -> Ok
            | false -> ValidationError message
        { validators with Restraints = customRestraint :: validators.Restraints }

let stringRestraint = StringValidationBuilder()