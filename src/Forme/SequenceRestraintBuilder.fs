namespace Forme

open Common

[<AutoOpen>]
module SequenceRestraintBuilder =
    
    type SequenceRestraint<'T> = 'T -> ValidationResult

    type SeqeunceRestraints<'T> = SequenceRestraint<'T> list

    type SequenceRestraintBuilder<'T>() =
        member __.Yield _ : SeqeunceRestraints<'T> = List.empty
        member __.Run validations sx =
            let applyValidations (validations: SeqeunceRestraints<'T>) (s:'T) =
                validations
                |> List.map (fun v -> v s)
                |> collectErrors
            
            sx
            |> Seq.map (fun s -> applyValidations validations s)
            |> Seq.toList
            |> collectErrors
            
        [<CustomOperation "eachElementMust">]
        member __.EachItemMust<'T> (validations: SeqeunceRestraints<'T>, restraint) =
            restraint :: validations

    let validSeq<'T> = SequenceRestraintBuilder<'T>()