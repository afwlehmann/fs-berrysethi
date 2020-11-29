module BerrySethi.NFA

open System

type NFA<'State, 'T when 'State: comparison and 'T: comparison>(Delta: Map<'State * 'T, Set<'State>>,
                                                                q0: 'State,
                                                                F: Set<'State>) =

    let Delta = Delta
    let F = F
    let q0 = q0

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

    member x.Generate =
        let rnd = Random()

        let transitionsFrom state =
            Map.toSeq Delta
            |> Seq.filter (fun ((from, _), _) -> from = state)
            |> Seq.collect (fun (k, targetStates) -> targetStates |> Seq.map (fun state -> (k, state)))
            |> Seq.toList

        let randomTransition q =
            let transitions = transitionsFrom q
            if transitions.IsEmpty then
                None
            else
                let index = rnd.Next(transitions.Length)
                Some transitions.[index]

        let rec go acc q =
            if F.Contains(q) && rnd.Next(2) = 1 then
                acc
            else
                match randomTransition q with
                | Some ((_, sigma), q') -> go (sigma :: acc) q'
                | None -> assert F.Contains(q); acc

        go [] q0 |> String.Concat
