namespace Forme

[<AutoOpen>]
module IntRestraintBuilder =

    type IntRestraint = int -> ValidationResult

    type IntRestraints = IntRestraint list

    type IntRestraintBuilder() =
        member __.Yield _ : IntRestraints = List.Empty
        member __.Run validations i =
            validations
            |> List.map (fun v -> v i)
            |> collectErrors

        [<CustomOperation "atLeast">]
        member __.AtLeast(validators: IntRestraints, minimum) =
            let atLeast min i = 
                if i >= min then Ok else ValidationError [{ Message = (sprintf "must be at least %i" min) }]
            (atLeast minimum) :: validators

        [<CustomOperation "atMost">]
        member __.AtMost(validators: IntRestraints, maximum) =
            let atMost max i =
                if i <= max then Ok else ValidationError [{ Message = (sprintf "must be at most %i" max) }]
            (atMost maximum) :: validators

        [<CustomOperation "notZero">]
        member __.NotZero(validators: IntRestraints) =
            let mustNotBeZero i =
                if i <> 0 then Ok else ValidationError [{ Message = "must not equal zero" }]
            mustNotBeZero :: validators

        [<CustomOperation "equal">]
        member __.Equal (validators: IntRestraints, mustEqual) =
            let equals mustBe i =
                match mustBe = i with
                | true -> Ok
                | false -> ValidationError[{ Message = sprintf "Must equal %i" mustBe }]
            (equals mustEqual) :: validators

    let validInt = IntRestraintBuilder()