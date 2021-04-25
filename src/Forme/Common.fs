namespace Forme
open FsToolkit.ErrorHandling

[<AutoOpen>]
module Common =
    type Error = { Message: string }
    
    type ValidationResult =
        | ValidationError of Error list
        | Ok

    let takeError result =
        result
        |> function
            | ValidationError e -> Some e
            | _ -> None

    let takeErrors lst =
        lst |> List.choose takeError

    let collectErrors evaluated =
        evaluated
        |> takeErrors
        |> function
            | [] -> Ok
            | errors ->
                errors
                |> List.concat
                |> ValidationError