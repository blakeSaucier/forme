namespace Forme

open Common

[<AutoOpen>]
module SequenceRestraintBuilder =
    
    type SequenceRestraint<'T> = 'T -> ValidationResult

    type SeqeunceRestraints<'T> = SequenceRestraint<'T> list

    type SequenceRestraintBuilder<'T>() =
        member __.Yield _ : SeqeunceRestraints<'T> = List.empty
        member __.Run (validations: SeqeunceRestraints<'T>) (sx:seq<'T>) =
            let applyValidations (validations: SeqeunceRestraints<'T>) (s:'T) =
                validations
                |> List.map (fun v -> v s)
                |> collectErrors
            
            sx
            |> Seq.map (fun s -> applyValidations validations s)
            |> Seq.toList
            |> collectErrors
            
        [<CustomOperation "eachElementMust">]
        member __.EachItemMust<'T> (validations: SeqeunceRestraints<'T>, restraint:('T -> ValidationResult)) =
            restraint :: validations

    let validSeq<'T> = SequenceRestraintBuilder<'T>()