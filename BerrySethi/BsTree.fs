module BerrySethi.BsTree

open NFA

type Lf<'s> =
    | Lf of int * 's
    | Root

type BsTree<'s> =
    | Leaf of Lf<'s>
    | Concat of BsTree<'s> * BsTree<'s>
    | Or of BsTree<'s> * BsTree<'s>
    | Asterisk of BsTree<'s>
    | QuestionMark of BsTree<'s>

let rec empty =
    function
    | Leaf _ -> false
    | Concat (l, r) -> empty l && empty r
    | Or (l, r) -> empty l || empty r
    | Asterisk _ -> true
    | QuestionMark _ -> true

let rec first =
    function
    | Leaf lf -> Set.singleton lf
    | Concat (l, r) -> if empty l then Set.union (first l) (first r) else first l
    | Or (l, r) -> Set.union (first l) (first r)
    | Asterisk n -> first n
    | QuestionMark n -> first n

let rec last =
    function
    | Leaf lf -> Set.singleton lf
    | Concat (l, r) -> if empty r then Set.union (last l) (last r) else last r
    | Or (l, r) -> Set.union (last l) (last r)
    | Asterisk n -> last n
    | QuestionMark n -> last n

let next root node =
    let rec next' root' acc =
        match root' with
        | _ when root' = node -> (acc, true)
        | Leaf _ -> (acc, false)
        | Concat (l, r) ->
            match next' r acc with
            | (_, true) as result -> result
            | _ -> next' l (Set.union (first r) (if empty r then acc else Set.empty))
        | Or (l, r) ->
            match next' l acc with
            | (_, true) as result -> result
            | _ -> next' r acc
        | Asterisk n -> next' n (Set.union (first n) acc)
        | QuestionMark n -> next' n acc

    next' root Set.empty
    |> fun (result, _) -> result

let rec toList =
    function
    | Concat (l, r) as self -> List.append (toList l) (self :: (toList r))
    | Or (l, r) as self -> List.append (toList l) (self :: (toList r))
    | self -> [ self ]

let toNFA<'a when 'a: comparison> root =
    let leaves =
        let rec go =
            function
            | Leaf _ as result -> Set.singleton result
            | Concat (l, r) -> Set.union (go l) (go r)
            | Or (l, r) -> Set.union (go l) (go r)
            | Asterisk n -> go n
            | QuestionMark n -> go n

        go root

    let finalStates =
        Set.union (last root) (if empty root then Set.singleton Root else Set.empty)

    let transitions =
        let foo: seq<(Lf<'a> * 'a) * Set<Lf<'a>>> =
            first root
            |> Seq.map (fun (Lf (_, s) as self) -> ((Root, s), self))
            |> Seq.groupBy (fun (key, _) -> key)
            |> Seq.map (fun (key, value) -> (key, value |> Seq.map (fun (_, node) -> node) |> Set.ofSeq))

        let bar: seq<(Lf<'a> * 'a) * Set<Lf<'a>>> =
            leaves
            |> Seq.map (fun l -> (l, next root l))
            |> Seq.collect (fun ((Leaf lf as l), targets) ->
                targets |> Seq.map (fun (Lf (_, s) as self) -> ((lf, s), self)))
            |> Seq.groupBy (fun (key, _) -> key)
            |> Seq.map (fun (key, value) -> (key, value |> Seq.map (fun (_, node) -> node) |> Set.ofSeq))

        Seq.append foo bar |> Map.ofSeq

    NFA(transitions, Root, finalStates)