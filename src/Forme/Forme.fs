namespace Forme

open Forme.Common
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns

type PropGet<'T, 'U> = Expr<'T -> 'U>

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

    let propNameGetter (getter:Expr<'T -> 'U>) =
        let rec matchPropName expr =
            match expr with
            | Patterns.PropertyGet(_, p, _) -> p.Name
            | Lambda(_, ex) -> matchPropName ex
            | _ -> failwith "Unsupported Expression"
        matchPropName getter

    let propGetter (getter:Expr<'T -> 'U>) =
        let rec matchPropGet expr =
            match expr with
            | Patterns.PropertyGet(_, p, _) -> (fun (t:'T) -> p.GetValue(t) :?> 'U)
            | Lambda(_, ex) -> matchPropGet ex
            | _ -> failwith "Unsupported Expression"
        matchPropGet getter

    type ValidatorBuilder<'T>() =
        member __.Yield _ = { Validations = List.Empty }
        member __.Run (modelValidator: ModelValidator) (t:'T) =
            let boxed = box t
            let results = modelValidator.Validations |> List.map (fun r -> (r.Restraint boxed))
            match getFailures results with
            | [] -> Ok
            | errs -> reduceErrors errs

        [<CustomOperation "restrain">]
        member __.Restrain   (modelValidator: ModelValidator,
                            ([<ReflectedDefinition>] getter: PropGet<'T, 'U>),
                            (validation: 'U -> ValidationResult)) =
            let propName = propNameGetter getter
            let typedPropGetter = propGetter getter
            let newRestraint = fun (value: obj) -> (typedPropGetter >> validation) (value :?> 'T)
            { Validations = { Restraint = newRestraint } :: modelValidator.Validations }

    let validateFor<'T> = ValidatorBuilder<'T>()