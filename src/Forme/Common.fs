namespace Forme

[<AutoOpen>]
module Common =
    type Error = { Message: string }

    type ValidationResult =
        | ValidationError of Error
        | Ok

    let joinErrorMessages errors =
        let allErrors = 
            errors
            |> List.map (fun e -> e.Message)
            |> String.concat "; "
        ValidationError { Message = allErrors }

    let collectErrors evaluated =
        evaluated
        |> List.choose
            (function
            | ValidationError err -> Some err
            | _ -> None)
        |> function
            | [] -> Ok
            | errors -> joinErrorMessages errors

    