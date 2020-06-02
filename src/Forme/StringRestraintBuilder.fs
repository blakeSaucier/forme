namespace Forme

open System
open Common

[<AutoOpen>]
module StringValidationBuilder =

    type StringRestraint = string -> ValidationResult

    type StringRestraints = StringRestraint list

    type StringRestraintBuilder() =
        member __.Yield _ : StringRestraints = List.Empty
        member __.Run validations str =
            validations
            |> List.map (fun v -> v str)
            |> collectErrors

        /// String cannot be null or whitespace
        [<CustomOperation "notEmpty">]
        member __.NotEmpty(validators: StringRestraints) =
            let mustNotBeEmpty s =
                s 
                |> String.IsNullOrWhiteSpace
                |> function
                    | true -> ValidationError { Message = "must not be empty" }
                    | false -> Ok
            mustNotBeEmpty :: validators

        /// The string's length must be less than or equal to the specified length
        [<CustomOperation "notLongerThan">]
        member __.NotLongerThan(validators: StringRestraints, length) =
            let notLongerThanLength length str = 
                str
                |> String.length <= length
                |> function
                    | true -> Ok
                    | false -> ValidationError { Message = sprintf "must not be longer than %i" length }
            notLongerThanLength length :: validators

        /// The string's length must be at least as long as the specified length
        [<CustomOperation "notShorterThan">]
        member __.NotShorterThan(validators: StringRestraints, length) =
            let atLeastAsLong length str =
                str
                |> String.length >= length
                |> function 
                    | true -> Ok
                    | false -> ValidationError { Message = sprintf "must be at least as long as %i" length }
            atLeastAsLong length :: validators

        /// The string must have the specified length
        [<CustomOperation "hasLengthOf">]
        member __.HasLength(validators: StringRestraints, length) =
            let hasLength length str = 
                str
                |> String.length = length
                |> function
                    | true -> Ok 
                    | false -> ValidationError { Message = sprintf "must have length %i" length }
            hasLength length :: validators

        /// The string must start with the supplied substring
        [<CustomOperation "startsWith">]
        member __.StartsWith(validators: StringRestraints, str) =
            let mustStartWith str (s:string) =
                str
                |> s.StartsWith
                |> function 
                    | true -> Ok
                    | false -> ValidationError { Message = sprintf "must start with '%s'" str }
            (mustStartWith str) :: validators

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
            mustBeParsableInt :: validators

        /// Describe a custom string validation
        [<CustomOperation "must">]
        member __.Must(validators: StringRestraints, (restraint: string -> bool), message) =
            let customRestraint str  =
                str
                |> restraint
                |> function
                    | true -> Ok
                    | false -> ValidationError { Message = message }
            customRestraint :: validators

    let validString = StringRestraintBuilder()