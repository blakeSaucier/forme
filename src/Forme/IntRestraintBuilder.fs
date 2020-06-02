module Forme.IntRestraintBuilder

open Common

type IntRestraint = int -> ValidationResult

type IntRestraints = { Restraints: IntRestraint list }

type IntRestraintBuilder() =
    member __.Yield _ = { Restraints = List.empty }
    member __.Run (validations: IntRestraints) i =
        validations.Restraints
        |> List.map (fun v -> v i)
        |> collectErrors

    [<CustomOperation "atLeast">]
    member __.AtLeast(validators: IntRestraints, minimum) =
        let atLeast min i = 
            if i >= min then Ok else ValidationError { Message = (sprintf "must be at least %i" min) }
        let partial = atLeast minimum
        { validators with Restraints = partial :: validators.Restraints }

    [<CustomOperation "atMost">]
    member __.AtMost(validators: IntRestraints, maximum) =
        let atMost max i =
            if i <= max then Ok else ValidationError { Message = (sprintf "must be at most %i" max) }
        let partial = atMost maximum
        { validators with Restraints = partial :: validators.Restraints }

    [<CustomOperation "notZero">]
    member __.NotZero(validators: IntRestraints) =
        let mustNotBeZero i =
            if i <> 0 then Ok else ValidationError { Message = "must not equal zero" }
        { validators with Restraints = mustNotBeZero :: validators.Restraints }

let validInt = IntRestraintBuilder()