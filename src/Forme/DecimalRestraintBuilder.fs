module Forme.DecimalRestraintBuilder

open Common

type DecimalRestraint = decimal -> ValidationResult

type DecimalRestraints = DecimalRestraint list

type DecimalRestraintBuilder () =
    member __.Yield _ : DecimalRestraints = List.empty
    member __.Run (validations: DecimalRestraints) d =
        validations
        |> List.map (fun v -> v d)
        |> collectErrors

    [<CustomOperation "atLeast">]
    member __.AtLeast (validations, minimum) =
        let atLeast min d =
            match d >= min with
            | true -> Ok
            | false -> ValidationError { Message = (sprintf "must be at least %M" min) }
        (atLeast minimum) :: validations

    [<CustomOperation "atMost">]
    member __.AtMost (validations: DecimalRestraints, maximum) =
        let atMost max d =
            match max <= d with
            | true -> Ok
            | false -> ValidationError { Message = (sprintf "must be at most %M" max) }
        (atMost maximum) :: validations

    [<CustomOperation "notZero">]
    member __.NotZero (validations) =
        let notZero d =
            match d = 0M with
            | true -> ValidationError { Message = "must not equal zero" }
            | false -> Ok
        notZero :: validations

let validDecimal = DecimalRestraintBuilder()