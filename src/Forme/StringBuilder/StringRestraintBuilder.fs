namespace Forme

open System
open Common
open StringHelper

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
                    | true -> ValidationError [{ Message = "must not be empty" }]
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
                    | false -> ValidationError [{ Message = sprintf "must not be longer than %i" length }]
            notLongerThanLength length :: validators

        /// The string's length must be at least as long as the specified length
        [<CustomOperation "notShorterThan">]
        member __.NotShorterThan(validators: StringRestraints, length) =
            let atLeastAsLong length str =
                str
                |> String.length >= length
                |> function 
                    | true -> Ok
                    | false -> ValidationError [{ Message = sprintf "must be at least as long as %i" length }]
            atLeastAsLong length :: validators

        /// The string must have the specified length
        [<CustomOperation "hasLengthOf">]
        member __.HasLength(validators: StringRestraints, length) =
            let hasLength length str =
                match str with
                | HasLength length _ -> Ok
                | _ -> ValidationError [{ Message = sprintf "must have length %i" length }]
            hasLength length :: validators

        /// The string must start with the supplied substring
        [<CustomOperation "startsWith">]
        member __.StartsWith(validators: StringRestraints, str) =
            let mustStartWith (str: string) (s:string) =
                match s with
                | StartsWith str _ -> Ok
                | _ -> ValidationError [{ Message = sprintf "must start with '%s'" str }]
            (mustStartWith str) :: validators

        /// The string must be parsable using Int32.TryParse
        [<CustomOperation "parsable_int">]
        member __.ParsableInt(validators: StringRestraints) =
            let mustBeParsableInt str =
                match str with
                | Int _ -> Ok
                | _ -> ValidationError [{ Message = sprintf "'%s' is not parsable as an Int32" str }]
            mustBeParsableInt :: validators

        /// The string must be parsable using Decimal.TryParse
        [<CustomOperation "parsable_decimal">]
        member __.ParsableDecimal(validators: StringRestraints) =
            let mustBeParsableDecimal str =
                match str with
                | Decimal _ -> Ok
                | _ -> ValidationError [{ Message = sprintf "'%s' is not parsable as a decimal" str }]
            mustBeParsableDecimal :: validators

        [<CustomOperation "equal">]
        member __.Equal (validators: StringRestraints, mustEqual) =
            let equals mustBe s =
                match String.Equals(mustBe, s, StringComparison.CurrentCulture) with
                | true -> Ok
                | false -> ValidationError [{ Message = sprintf "Must equal %s" mustBe }]
            (equals mustEqual) :: validators

        [<CustomOperation "equal">]
        member __.Equal (validators: StringRestraints, mustEqual, culture:StringComparison) =
            let equals mustBe s =
                match String.Equals(mustBe, s, culture) with
                | true -> Ok
                | false -> ValidationError [{ Message = sprintf "Must equal %s" mustBe }]
            (equals mustEqual) :: validators

        [<CustomOperation "email">]
        member __.Email (validators: StringRestraints) =
            let isEmail str =
                match str with
                | Email _ -> Ok
                | _ -> ValidationError [{ Message = sprintf "'%s' is not a valid email" str }]
            isEmail :: validators

        /// Describe a custom string validation
        [<CustomOperation "must">]
        member __.Must(validators: StringRestraints, (restraint: string -> bool), message) =
            let customRestraint str  =
                match (restraint str) with
                | true -> Ok
                | false -> ValidationError [{ Message = message }]
            customRestraint :: validators

    let validString = StringRestraintBuilder()