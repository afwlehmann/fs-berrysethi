module BerrySethi.NFA

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