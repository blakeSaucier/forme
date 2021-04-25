# Forme

Readable model validations for F#.

Example:

```fsharp
open Forme

type Person = 
    { FirstName: string
      LastName: string; 
      Age: int }

let aReasonableName = validString {
    notEmpty
    notLongerThan 100
    notShorterThan 3
}

let noMinorsOrSeniors = validInt {
    atLeast 19
    atMost 70
}

let canEnterNightClub = valid<Person> {
    rule (fun p -> p.FirstName) aReasonableName
    rule (fun p -> p.LastName)  aReasonableName
    rule (fun p -> p.Age)       noMinorsOrSeniors
}

person 
|> canEnterNightClub
|> function
    | Ok -> person
    | ValidationError error -> error.Message 
// etc

```
