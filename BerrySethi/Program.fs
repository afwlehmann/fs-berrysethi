open FParsec

open BerrySethi

[<EntryPoint>]
let main argv =
    match argv with
    | [| regex; input |] ->
        let parseResult = regex |> Parser.parseRegex

        let nfa =
            match parseResult with
            | Success (tree, _, _) -> tree |> BsTree.toNFA
            | Failure (msg, _, _) -> failwith msg

        if input |> Seq.toList |> nfa.Accept then 0 else 1
    | _ ->
        let basename =
            System.Diagnostics.Process.GetCurrentProcess().ProcessName

        failwith <| sprintf "Usage: %s <regex> <input>" basename
