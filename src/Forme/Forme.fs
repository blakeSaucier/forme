namespace Forme

open Forme.Common
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns

type PropGet<'T, 'U> = Expr<'T -> 'U>

type PropertyValidator = 
    { Restraint: obj -> ValidationResult
      FieldName: string }

type PropertyValidated =
    { Result: ValidationResult
      FieldName: string }

type ModelValidator = { Validations: PropertyValidator list }

module Validator =
    let private reduceErrors errors =
        let allErrors = 
            errors
            |> List.map (fun e -> e.Message)
            |> String.concat "\n"
        ValidationError { Message = allErrors }

    let private getFailures results =
        results |> List.choose (fun v ->
            match v.Result with
            | ValidationError e -> Some { Message = sprintf "'%s' Failed Validation: %s" v.FieldName e.Message }
            | Ok -> None)

    let private propNameGetter (getter:Expr<'T -> 'U>) =
        let rec matchPropName expr =
            match expr with
            | Patterns.PropertyGet(_, p, _) -> p.Name
            | Lambda(_, ex) -> matchPropName ex
            | _ -> failwith "Unsupported Expression"
        matchPropName getter

    let private propGetter (getter:Expr<'T -> 'U>) =
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
            let results = 
                modelValidator.Validations 
                |> List.map (fun r -> { Result = (r.Restraint boxed); FieldName = r.FieldName })
            match getFailures results with
            | [] -> Ok
            | errs -> reduceErrors errs

        [<CustomOperation "restrain">]
        member __.Restrain   (modelValidator: ModelValidator,
                            ([<ReflectedDefinition>] getter: PropGet<'T, 'U>),
                            (validation: 'U -> ValidationResult)) =
            let propName = propNameGetter getter
            let typedPropGetter = propGetter getter
            let validationFunction = fun (value: obj) -> (typedPropGetter >> validation) (value :?> 'T)
            let restraint = { Restraint = validationFunction; FieldName = propName }
            { Validations = restraint :: modelValidator.Validations }

    let validateFor<'T> = ValidatorBuilder<'T>()