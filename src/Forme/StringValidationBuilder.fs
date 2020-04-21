[<AutoOpen>]
module Forme.StringValidationBuilder

open System

type StringRestraint = string -> bool

type StringValidation = { Restraints: StringRestraint list }

type StringValidationBuilder() =
    member __.Yield _ = { Restraints = List.Empty }
    member __.Run(validation: StringValidation) (str: string) =
        let res = validation.Restraints |> List.map (fun v -> v str)
        match res with
        | [] -> true
        | _ -> res |> List.reduce (&&)
    
    /// String cannot be null or whitespace
    [<CustomOperation "notEmpty">]
    member __.NotEmpty(validators: StringValidation) =
        let mustNotBeEmpty s = s |> String.IsNullOrWhiteSpace |> not
        let appended = mustNotBeEmpty :: validators.Restraints
        { validators with Restraints = appended }

    /// The string's length must be less than or equal to the specified length
    [<CustomOperation "notLongerThan">]
    member __.NotLongerThan(validators: StringValidation, length)  =
        let notLongerThanLength length str = str |> String.length <= length
        let restriction = notLongerThanLength length
        let appended = restriction :: validators.Restraints
        { validators with Restraints = appended }

    /// Describe a custom string validation
    [<CustomOperation "must">]
    member __.Must(validators: StringValidation, (restraint: string -> bool)) =
        let allValidators = restraint :: validators.Restraints
        { validators with Restraints = allValidators }

let stringRestraint = StringValidationBuilder()