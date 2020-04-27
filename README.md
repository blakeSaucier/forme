# Forme

Readable model validations for F#.

Example:

```fsharp

type Person = 
    { FirstName: string
      LastName: string; 
      Age: int }

let validName = stringRestraint {
    notEmpty
    notLongerThan 100
    notShorterThan 3
}

let noMinorsOrSeniors = intRestraint {
    atLeast 19
    atMost 70
}

let canEnterNightClub = validateFor<Person> {
    it (fun p -> p.FirstName)   mustBe validName
    it (fun p -> p.LastName)    mustBe validName
    it (fun p -> p.Age)         mustBe noMinorsOrSeniors
}

match person |> canEnterNightClub with
| Ok -> person
| ValidationError error -> error.Message // etc

```
