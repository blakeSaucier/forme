module Forme.StringValidationBuilder

open System
open Common

type StringRestraint = string -> ValidationResult

type StringRestraints = { Restraints: StringRestraint list }

type StringRestraintBuilder() =
    member __.Yield _ = { Restraints = List.Empty }
    member __.Run(validation: StringRestraints) (str: string) =
        let evaluated = validation.Restraints |> List.map (fun v -> v str)
        let failures = evaluated |> List.choose (fun v ->
            match v with
            | ValidationError err -> Some err
            | _ -> None)
        match failures with
            | [] -> Ok
            | errors -> joinErrorMessages errors

    /// String cannot be null or whitespace
    [<CustomOperation "notEmpty">]
    member __.NotEmpty(validators: StringRestraints) =
        let mustNotBeEmpty s =
            match s |> String.IsNullOrWhiteSpace with
            | true -> ValidationError { Message = "must not be empty" }
            | false -> Ok
        { validators with Restraints = mustNotBeEmpty :: validators.Restraints }

    /// The string's length must be less than or equal to the specified length
    [<CustomOperation "notLongerThan">]
    member __.NotLongerThan(validators: StringRestraints, length) =
        let notLongerThanLength length str = 
            str
            |> String.length <= length
            |> function
                | true -> Ok
                | false -> ValidationError { Message = sprintf "must not be longer than %i" length }
        { validators with Restraints = notLongerThanLength length :: validators.Restraints }

    /// The string's length must be at least as long as the specified length
    [<CustomOperation "notShorterThan">]
    member __.NotShorterThan(validators: StringRestraints, length) =
        let atLeastAsLong length str =
            str
            |> String.length >= length
            |> function 
                | true -> Ok
                | false -> ValidationError { Message = sprintf "must be at least as long as %i" length }
        { validators with Restraints = atLeastAsLong length :: validators.Restraints }

    /// The string must have the specified length
    [<CustomOperation "hasLengthOf">]
    member __.HasLength(validators: StringRestraints, length) =
        let hasLength length str = 
            if str |> String.length = length then Ok 
            else ValidationError { Message = sprintf "must have length %i" length }
        { validators with Restraints = hasLength length :: validators.Restraints }

    /// The string must start with the supplied substring
    [<CustomOperation "startsWith">]
    member __.StartsWith(validators: StringRestraints, str) =
        let mustStartWith str (s:string) =
            str
            |> s.StartsWith
            |> function 
                | true -> Ok
                | false -> ValidationError { Message = sprintf "must start with '%s'" str }
        { validators with Restraints = (mustStartWith str) :: validators.Restraints }

    /// The string must be parsable using Int32.TryParse
    [<CustomOperation "parsable_int">]
    member __.ParsableInt(validators: StringRestraints) =
        let mustBeParsableInt str =
            str
            |> Int32.TryParse
            |> fun (res, _) -> res
            |> function
                | true -> Ok
                | false -> ValidationError { Message = sprintf "'%s' is not parsable as an Int32" str }
        { validators with Restraints = mustBeParsableInt :: validators.Restraints }

    /// Describe a custom string validation
    [<CustomOperation "must">]
    member __.Must(validators: StringRestraints, (restraint: string -> bool), message) =
        let customRestraint str  =
            match restraint str with
            | true -> Ok
            | false -> ValidationError { Message = message }
        { validators with Restraints = customRestraint :: validators.Restraints }

let validString = StringRestraintBuilder()