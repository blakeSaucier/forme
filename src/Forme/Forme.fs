namespace Forme

open Forme.Common
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns

[<AutoOpen>]
module Validator =

    type PropGet<'T, 'U> = Expr<'T -> 'U>

    type PropertyValidator =
        { Restraint: obj -> ValidationResult
          FieldName: string }

    type PropertyValidated =
        { Result: ValidationResult
          FieldName: string }

    type ModelValidator = PropertyValidator list

    let combine (errors: Error list) =
        errors
        |> List.map (fun e -> e.Message)
        |> String.concat "; "

    let private getFailures results =
        results
        |> List.choose (fun v ->
            match v.Result with
            | ValidationError e -> Some { Message = sprintf "'%s' %s" v.FieldName (combine e) }
            | Ok -> None)

    let private propertyName (getter:Expr<'T -> 'U>) =
        let rec matchPropName expr =
            match expr with
            | PropertyGet(_, p, _) -> p.Name
            | Lambda(_, ex) -> matchPropName ex
            | _ -> failwith "Unsupported Expression"
        matchPropName getter

    let private propGetter (getter:Expr<'T -> 'U>) =
        let rec matchPropGet expr =
            match expr with
            | PropertyGet(_, p, _) -> fun (t:'T) -> p.GetValue(t) :?> 'U
            | Lambda(_, ex) -> matchPropGet ex
            | _ -> failwith "Unsupported Expression"
        matchPropGet getter

    let private runValidation validations t =
        let boxed = box t
        validations
        |> List.map (fun r ->
            { Result = (r.Restraint boxed)
              FieldName = r.FieldName })
        |> getFailures
        |> function
            | [] -> Ok
            | errors -> ValidationError errors

    type ValidatorBuilder<'T>() =
        member _.Yield _ : ModelValidator = List.Empty
        member _.Run modelValidator t = runValidation modelValidator t

        [<CustomOperation "rule">]
        member _.Restrain   (modelValidator: ModelValidator,
                            ([<ReflectedDefinition>] getter: PropGet<'T, 'U>),
                            (validation: 'U -> ValidationResult)) =
            let propName = propertyName getter
            let typedPropGetter = propGetter getter
            let validationFunction = fun (value: obj) -> (typedPropGetter >> validation) (value :?> 'T)
            { Restraint = validationFunction; FieldName = propName } :: modelValidator

    let valid<'T> = ValidatorBuilder<'T>()
