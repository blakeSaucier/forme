namespace Forme

[<AutoOpen>]
module DecimalRestraintBuilder = 
    type DecimalRestraint = decimal -> ValidationResult

    type DecimalRestraints = DecimalRestraint list

    type DecimalRestraintBuilder () =
        member _.Yield _ : DecimalRestraints = List.empty
        member _.Run validations (d: decimal) =
            validations
            |> List.map (fun v -> v d)
            |> collectErrors

        [<CustomOperation "atLeast">]
        member _.AtLeast (validations, minimum) =
            let atLeast min d =
                match d >= min with
                | true -> Ok
                | false -> ValidationError [{ Message = (sprintf "must be at least %M" min) }]
            (atLeast minimum) :: validations

        [<CustomOperation "atMost">]
        member _.AtMost (validations: DecimalRestraints, maximum) =
            let atMost max d =
                match max <= d with
                | true -> Ok
                | false -> ValidationError [{ Message = (sprintf "must be at most %M" max) }]
            (atMost maximum) :: validations

        [<CustomOperation "notZero">]
        member _.NotZero (validations) =
            let notZero d =
                match d = 0M with
                | true -> ValidationError [{ Message = "must not equal zero" }]
                | false -> Ok
            notZero :: validations

        [<CustomOperation "equal">]
        member _.Equal (validations: DecimalRestraints, mustEqual) =
            let equals mustBe d =
                match mustBe = d with
                | true -> Ok
                | false -> ValidationError [{ Message = sprintf "Must equal %M" mustBe }]
            (equals mustEqual) :: validations

    let validDecimal = DecimalRestraintBuilder()

module validDecimal =
    let notZeroDecimal = validDecimal { notZero }