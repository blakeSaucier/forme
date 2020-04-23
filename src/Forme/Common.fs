namespace Forme
module Common =
    type ValidationError = { Message: string }

    type ValidationResult =
        | ValidationError of string
        | Ok

    type WeakStringValidator = obj -> ValidationResult