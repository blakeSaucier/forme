module Forme.IntRestraintBuilder

open Common

type IntRestraint = int -> ValidationResult

type IntRestraints = { Restraints: IntRestraint list }

type IntRestraintBuilder() =
    member __.Yield _ = { Restraints = List.empty }
    member __.Run (validations: IntRestraints) i =
        let evaluated = validations.Restraints |> List.map (fun v -> v i)
        let failures = evaluated |> List.choose (fun v ->
            match v with
            | ValidationError err -> Some err
            | _ -> None)
        match failures with
            | [] -> Ok
            | errors -> joinErrorMessages errors

    [<CustomOperation "atLeast">]
    member __.AtLeast(validators: IntRestraints, minimum) =
        let atLeast min i = 
            if i >= min then Ok else ValidationError { Message = (sprintf "Must be greater than %i" min) }
        let partial = atLeast minimum
        { validators with Restraints = partial :: validators.Restraints }

    [<CustomOperation "atMost">]
    member __.AtMost(validators: IntRestraints, maximum: int) =
        let atMost max i =
            if i <= max then Ok else ValidationError { Message = (sprintf "Must be less than or equal to %i" max) }
        let partial = atMost maximum
        { validators with Restraints = partial :: validators.Restraints }

    [<CustomOperation "notZero">]
    member __.NotZero(validators: IntRestraints) =
        let mustNotBeZero i =
            if i <> 0 then Ok else ValidationError { Message = "Must not equal zero" }
        { validators with Restraints = mustNotBeZero :: validators.Restraints }

let intRestraint = IntRestraintBuilder()