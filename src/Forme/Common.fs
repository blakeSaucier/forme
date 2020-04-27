namespace Forme
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
    