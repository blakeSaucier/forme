[<AutoOpen>]
module Forme.StringValidationBuilder

open System

type ValidationError = { Message: string }

type StringRestraint = string -> Result<string, ValidationError>

type StringValidation = { Restraints: StringRestraint list }

type StringValidationBuilder() =
    member __.Yield _ = { Restraints = List.Empty }
    member __.Run(validation: StringValidation) (str: string) =
        let evaluated = validation.Restraints |> List.map (fun v -> v str)
        let failures = evaluated |> List.choose (fun v ->
            match v with
            | Error err -> Some err
            | _ -> None)
        match failures with
        | [] -> Ok str
        | _ -> Error failures

    /// String cannot be null or whitespace
    [<CustomOperation "notEmpty">]
    member __.NotEmpty(validators: StringValidation) =
        let mustNotBeEmpty s =
            match s |> String.IsNullOrWhiteSpace with
            | true -> Error { Message = "Must not be empty" }
            | false -> Ok s
        { validators with Restraints = mustNotBeEmpty :: validators.Restraints }

    /// The string's length must be less than or equal to the specified length
    [<CustomOperation "notLongerThan">]
    member __.NotLongerThan(validators: StringValidation, length)  =
        let notLongerThanLength length str = 
            match str |> String.length <= length with
            | true -> Ok str
            | false -> Error { Message = sprintf "must not be longer than %i" length }
        { validators with Restraints = notLongerThanLength length :: validators.Restraints }

    /// Describe a custom string validation
    [<CustomOperation "must">]
    member __.Must(validators: StringValidation, (restraint: string -> bool), message) =
        let customRestraint str  =
            match restraint str with
            | true -> Ok str
            | false -> Error { Message = message }
        { validators with Restraints = customRestraint :: validators.Restraints }

let stringRestraint = StringValidationBuilder()