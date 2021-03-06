﻿namespace Forme

module internal StringHelper =
    open System.Text.RegularExpressions
    open System.Net.Mail
    open System
    
    let (|Int|_|) str =
        match Int32.TryParse str with
        | true, i -> Some i
        | _ -> None

    let (|Decimal|_|) str =
        match Decimal.TryParse str with
        | true, d -> Some d
        | _ -> None

    let (|StartsWith|_|) (startsWith: string) (str: string) =
        match str.StartsWith startsWith with
        | true -> Some str
        | false -> None

    let (|HasLength|_|) (l:int) str =
        if String.length str = l
            then Some l
        else None
    
    let (|Email|_|) emailInput =
        try
            Some (MailAddress emailInput)
        with
            | _ -> None

    let (|Regex|_|) (pattern:string) (str:string) =
        let res = Regex.Match(str, pattern)
        if res.Success then Some(List.tail [for x in res.Groups-> x.Value])
        else None
