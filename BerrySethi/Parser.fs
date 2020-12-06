module BerrySethi.Parser

open FParsec

open BsTree

type UserState = int

[<AutoOpen>]
module private Internal =
    (*
        E -> ME | M
        M -> T* | T? | T
        T -> σ | (E|E)
    *)

    let pExpr, pExprImpl =
        createParserForwardedToRef<BsTree<char>, UserState> ()

    let pSigma: Parser<BsTree<char>, int> =
        asciiLetter
        <|> digit
        .>>. getUserState
        >>= fun (input, state) -> updateUserState (fun state -> state + 1) >>% Leaf(Lf(state, input))

    let pTerm =
        pSigma <|> (pchar '(' >>. pExpr .>> pchar '|' .>>. pExpr .>> pchar ')' |>> Or)

    let pMult =
        pTerm
        .>>. (opt <| anyOf "*?")
        |>> function
        | term, Some '*' -> Asterisk term
        | term, Some '?' -> QuestionMark term
        | term, _ -> term

    do pExprImpl
       := pMult
       .>>. (opt pExpr)
       |>> function
       | mult, Some expr -> Concat(mult, expr)
       | mult, None -> mult

let parseRegex regex =
    let initialState = 1
    runParserOnString pExpr initialState "berry-sethi" regex