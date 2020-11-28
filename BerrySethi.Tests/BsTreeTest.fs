module ``BerrySethi BsTree Tests``

open System.Collections
open Xunit
open FsCheck.Xunit

open BerrySethi.BsTree

type BsTreeProps() =
    // (a|b)*a(a|b)b?
    let exampleTree: BsTree<char> =
        Concat
            (Asterisk(Or(Leaf(Lf(0, 'a')), Leaf(Lf(1, 'b')))),
             Concat(Leaf(Lf(2, 'a')), Concat(Or(Leaf(Lf(3, 'a')), Leaf(Lf(4, 'b'))), QuestionMark(Leaf(Lf(5, 'b'))))))

    [<Fact>]
    let ``first yields the correct result for each node of the exampleTree`` () =
        let expected =
            [ set [ Lf(0, 'a'); Lf(1, 'b') ]
              set [ Lf(0, 'a')
                    Lf(1, 'b')
                    Lf(2, 'a') ]
              set [ Lf(2, 'a') ]
              set [ Lf(2, 'a') ]
              set [ Lf(3, 'a') ]
              set [ Lf(3, 'a'); Lf(4, 'b') ]
              set [ Lf(4, 'b') ]
              set [ Lf(3, 'a'); Lf(4, 'b') ]
              set [ Lf(5, 'b') ] ]

        Assert.Equal<IEnumerable>(expected, toList exampleTree |> List.map first)

    [<Fact>]
    let ``last yields the correct result for each node of the exampleTree`` () =
        let expected =
            [ set [ Lf(0, 'a'); Lf(1, 'b') ]
              set [ Lf(3, 'a')
                    Lf(4, 'b')
                    Lf(5, 'b') ]
              set [ Lf(2, 'a') ]
              set [ Lf(3, 'a')
                    Lf(4, 'b')
                    Lf(5, 'b') ]
              set [ Lf(3, 'a') ]
              set [ Lf(3, 'a'); Lf(4, 'b') ]
              set [ Lf(4, 'b') ]
              set [ Lf(3, 'a')
                    Lf(4, 'b')
                    Lf(5, 'b') ]
              set [ Lf(5, 'b') ] ]

        Assert.Equal<IEnumerable>(expected, toList exampleTree |> List.map last)

    [<Fact>]
    let ``next yields the correct result for each node of the exampleTree`` () =
        let expected =
            [ (set [ Lf(2, 'a') ])
              (set [])
              (set [ Lf(3, 'a'); Lf(4, 'b') ])
              (set [])
              (set [ Lf(5, 'b') ])
              (set [ Lf(5, 'b') ])
              (set [ Lf(5, 'b') ])
              (set [])
              (set []) ]

        Assert.Equal<IEnumerable>(expected, toList exampleTree |> List.map (next exampleTree))

    [<Property>]
    member x.``empty yields the correct result``(root: BsTree<char>) =
        empty root =
            match root with
            | Leaf _ -> false
            | Concat (l, r) -> empty l && empty r
            | Or (l, r) -> empty l || empty r
            | _ -> true

    [<Theory>]
    [<InlineData("aa")>]
    [<InlineData("ab")>]
    [<InlineData("aab")>]
    [<InlineData("abb")>]
    [<InlineData("bab")>]
    [<InlineData("babb")>]
    member x.``toNFA builds an NFA that accepts the corresponding regular expression``(input: string) =
        // (a|b)*a(a|b)b?
        let nfa = toNFA exampleTree
        input |> Seq.toList |> nfa.Accept |> Assert.True
