module ``BerrySethi NFA Tests``

open FsCheck.Xunit
open Xunit
open FsCheck
open System.Text.RegularExpressions

open BerrySethi.NFA

type NFAProps() =
    let exampleNFA =
        let Delta =
            Map.ofList [ (("q0", '0'), set [ "q0" ])
                         (("q0", '1'), set [ "q0"; "q1" ])
                         (("q1", '0'), set [ "q2" ])
                         (("q1", '1'), set [ "q2" ])
                         (("q2", '0'), set [ "q3" ])
                         (("q2", '1'), set [ "q3" ])
                         (("q3", '0'), set [ "q4" ])
                         (("q3", '1'), set [ "q4" ]) ]

        NFA(Delta, "q0", Set.singleton "q4")

    let validInputs =
        [ "1111"; "01111"; "11111" ] |> List.map Seq.toList

    [<Fact>]
    let ``NFA accepts valid input`` () =
        validInputs
        |> Seq.map Seq.toList
        |> Seq.map exampleNFA.Accept
        |> Seq.iter Assert.True

    [<Theory>]
    [<InlineData("")>]
    [<InlineData("11x11")>]
    [<InlineData("01121")>]
    [<InlineData("1111ya1")>]
    let ``NFA rejects invalid input`` (input: string) =
        input |> Seq.toList |> exampleNFA.Accept |> Assert.False

    [<Property>]
    let ``NFA generates sequences matching a regex for the same language`` () =
        Prop.forAll (Arb.fromGen <| gen { return exampleNFA.Generate() }) (fun generated ->
            Regex.IsMatch(generated, "(0|1)*1(0|1){3}"))
