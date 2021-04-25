namespace Forme

open Common

[<AutoOpen>]
module SequenceRestraintBuilder =
    
    type SequenceRestraint<'T> = 'T -> ValidationResult

    type SeqeunceRestraints<'T> = SequenceRestraint<'T> list

    type SequenceRestraintBuilder<'T>() =
        member _.Yield _ : SeqeunceRestraints<'T> = List.empty
        member _.Run validations sx =
            let applyValidations (validations: SeqeunceRestraints<'T>) (s:'T) =
                validations
                |> List.map (fun v -> v s)
                |> collectErrors
            
            sx
            |> Seq.map (fun s -> applyValidations validations s)
            |> Seq.toList
            |> collectErrors
            
        [<CustomOperation "eachElementMust">]
        member _.EachItemMust<'T> (validations: SeqeunceRestraints<'T>, restraint) =
            restraint :: validations

    let validSeq<'T> = SequenceRestraintBuilder<'T>()