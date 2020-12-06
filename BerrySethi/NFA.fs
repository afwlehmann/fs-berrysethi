module BerrySethi.NFA

open System

type NFA<'State, 'T when 'State: comparison and 'T: comparison>(Delta: Map<'State * 'T, Set<'State>>,
                                                                q0: 'State,
                                                                F: Set<'State>) =

    let Delta = Delta
    let F = F
    let q0 = q0

    // TODO: Check that there is no non-final state without outbound transitions

    member x.Accept input =
        let rec go q =
            function
            | h :: t ->
                match Delta.TryFind(q, h) with
                | Some targetStates -> targetStates |> Seq.map (fun q' -> go q' t) |> Seq.fold (||) false
                | _ -> false
            | [] when F.Contains q -> true
            | _ -> false

        go q0 input

    member x.Generate() =
        let rnd = Random()

        let transitionsByState: Map<'State, ('T * 'State) []> =
            Map.toSeq Delta
            |> Seq.map (fun ((source, input), targets) -> source, input, targets)
            |> Seq.groupBy (fun (s, _, _) -> s)
            |> Map.ofSeq
            |> Map.map (fun _ vs ->
                vs
                |> Seq.collect (fun (_, input, ts) -> ts |> Seq.map (fun t -> input, t))
                |> Seq.toArray)

        let randomTransition q =
            transitionsByState.TryFind q
            |> Option.filter (not << Array.isEmpty)
            |> Option.map (fun ts ->
                let index = rnd.Next(ts.Length)
                ts.[index])

        let rec go acc q =
            if F.Contains(q) && rnd.Next(2) = 1
            then acc
            else
                match randomTransition q with
                | Some (sigma, q') -> go (sigma :: acc) q'
                | None ->
                    assert F.Contains(q)
                    acc

        go [] q0 |> List.rev |> String.Concat
