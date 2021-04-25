namespace Forme

[<AutoOpen>]
module IntRestraintBuilder =

    type IntRestraint = int -> ValidationResult

    type IntRestraints = IntRestraint list

    type IntRestraintBuilder() =
        member _.Yield _ : IntRestraints = List.Empty
        member _.Run validations i =
            validations
            |> List.map (fun v -> v i)
            |> collectErrors

        [<CustomOperation "atLeast">]
        member _.AtLeast(validators: IntRestraints, minimum) =
            let atLeast min i = 
                if i >= min then Ok else ValidationError [{ Message = (sprintf "must be at least %i" min) }]
            (atLeast minimum) :: validators

        [<CustomOperation "atMost">]
        member _.AtMost(validators: IntRestraints, maximum) =
            let atMost max i =
                if i <= max then Ok else ValidationError [{ Message = (sprintf "must be at most %i" max) }]
            (atMost maximum) :: validators

        [<CustomOperation "notZero">]
        member _.NotZero(validators: IntRestraints) =
            let mustNotBeZero i =
                if i <> 0 then Ok else ValidationError [{ Message = "must not equal zero" }]
            mustNotBeZero :: validators

        [<CustomOperation "equal">]
        member _.Equal (validators: IntRestraints, mustEqual) =
            let equals mustBe i =
                match mustBe = i with
                | true -> Ok
                | false -> ValidationError[{ Message = sprintf "Must equal %i" mustBe }]
            (equals mustEqual) :: validators

    let validInt = IntRestraintBuilder()

module ValidInt =
    let equals i = validInt { equal i }
    let notZero = validInt { notZero }
