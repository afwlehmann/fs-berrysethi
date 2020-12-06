open FsCheck

open ``BerrySethi BsTree Tests``
open ``BerrySethi NFA Tests``

module Program =

    [<EntryPoint>]
    let main _ =
        Check.QuickAll<BsTreeProps>()
        Check.QuickAll<NFAProps>()
        0
