namespace Forme
module Common =
    type Error = { Message: string }

    type ValidationResult =
        | ValidationError of Error
        | Ok