open FsCheck

open ``BerrySethi BsTree Tests``

module Program =

    [<EntryPoint>]
    let main _ =
        Check.QuickAll<BsTreeProps>()
        0
