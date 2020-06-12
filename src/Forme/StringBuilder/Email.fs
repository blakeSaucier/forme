namespace Forme

module internal Email =
    open System.Net.Mail
    
    let (|Email|_|) emailInput =
        try
            Some (MailAddress emailInput)
        with
            | _ -> None
