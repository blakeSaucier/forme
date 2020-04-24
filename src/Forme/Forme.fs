namespace Forme

open Forme.Common

type PropertyValidator = { Restraint: obj -> ValidationResult }
type ModelValidator = { Validations: PropertyValidator list }

module Validator =
    let private reduceErrors errors =
        let allErrors = 
            errors
            |> List.map (fun e -> e.Message)
            |> List.fold (+) " "
        ValidationError { Message = allErrors }

    let private getFailures results =
        results |> List.choose (fun v ->
            match v with
            | ValidationError e -> Some e
            | Ok -> None)

    type ValidatorBuilder<'T>() =
        member __.Yield _ = { Validations = List.Empty }
        member __.Run (modelValidator: ModelValidator) (t:'T) =
            let boxed = box t
            let results = modelValidator.Validations |> List.map (fun r -> (r.Restraint boxed))
            match getFailures results with
            | [] -> Ok
            | errs -> reduceErrors errs

        [<CustomOperation "restrain">]
        member __.Restrain  (modelValidator: ModelValidator,
                            (property: 'T -> 'U),
                            (validation: 'U -> ValidationResult)) =
            let testing = fun(value: obj) -> (property >> validation) (value :?> 'T)
            let append = { Restraint = testing } :: modelValidator.Validations
            { Validations = append }

    let validateFor<'T> = ValidatorBuilder<'T>()