module GARbroFS
open System.Collections.Immutable

open NineP
open NineP.Server

let private pathCounter = MailboxProcessor<AsyncReplyChannel<uint64>>.Start(fun inbox ->
    let rec run maxpath = async {
        let! chan = inbox.Receive()
        chan.Reply maxpath
        return! run (maxpath+1UL)
    }
    run 0UL
)
pathCounter.Error.Add(fun x -> raise x)

type File(arc: GameRes.ArcFile, entry: GameRes.Entry) =
    let stat = Stat(
        qid = { Type = FileType.File; Ver = 0u; Path = pathCounter.PostAndReply(fun chan -> chan) },
        mode = 0o444u,
        atime = 0u,
        mtime = 0u,
        length = uint64 entry.Size, // TODO: use UnpackedSize if entry is PackedEntry (why is this a thing)
        name = entry.Name)

    interface IFile with
        member f.Stat =
            stat

        member f.Open(mode) =
            // TODO: handle automatically in NineP.Server based on stat.Mode
            if mode.HasFlag(OpenMode.Write) || mode.HasFlag(OpenMode.Rdwr) then
                Error "read only file system"
            else
                Ok (arc.OpenSeekableEntry(entry))

type Directory (stat: Stat, entries: Node []) =
    new(entries: Node [], name: string) =
        Directory(Stat(
            qid = { Type = FileType.Dir; Ver = 0u; Path = pathCounter.PostAndReply(fun chan -> chan) },
            mode = (0o555u ||| ((uint32 FileType.Dir) <<< 24)),
            atime = 0u,
            mtime = 0u,
            name = name), entries)

    interface IDirectory with
        member f.Stat =
            stat

        member d.Entries =
            entries :> Node seq

let private pathSeparators = [|'/'; '\\'|]

let private treeFromArc (isHierarchic: bool) (arc: GameRes.ArcFile) =
    match isHierarchic with
    | true -> "hierarchical"
    | false -> "flat"
    |> printfn "%s"
    arc.Dir
    |> Seq.map (fun entry -> File(arc, entry) :> IFile |> Node.File)
    |> Array.ofSeq
    |> fun entries -> Directory(entries, "/")
    :> IDirectory
    |> Node.Directory

// since impl.TryOpen doesn't actually behave like regular try* functions
let private realTryOpen (arcview: GameRes.ArcView) (impl: GameRes.ArchiveFormat) =
    try
        match impl.TryOpen arcview with
        | null -> Error (sprintf "%A: failed to open %s" impl arcview.Name)
        | x -> Ok x
    with
    | :? System.Exception as e -> Error e.Message

module SeqExt =
    /// Picks the first element from source for which chooser returns Ok x.
    /// Keeps track of last encountered error. If the input sequence is empty,
    /// defaultVal is returned.
    // FIXME: this seems unnecessarily convoluted, do we actually care about
    //        errors returned by non-Ok calls? if we do, wouldn't it make more
    //        sense to track all of them instead of just the last one?
    let rec pickOrElse chooser defaultVal source =
        match source |> Seq.tryHead with
        | None -> defaultVal
        | Some x ->
            let res = chooser x
            match res with
            | Ok v -> res
            | Error x -> pickOrElse chooser res (source |> Seq.tail)

let loadArchive path =
    // GameRes.ArcFile.TryOpen uses VFS (global mutable state), can't use it here
    let av = new GameRes.ArcView(path) // TODO: handle potential exceptions (maybe for the whole function, ReadUInt32 can throw too, LookupSignature might want to throw too)
    let signature = av.View.ReadUInt32(0L)
    // TODO: do extensions too? there might be formats without signatures
    GameRes.FormatCatalog.Instance.LookupSignature<GameRes.ArchiveFormat>(signature)
    |> SeqExt.pickOrElse
        (fun impl -> impl |> realTryOpen av |> Result.map (fun x -> impl.IsHierarchic, x))
        (sprintf "no handlers found for %s" path |> Error)
    |> Result.map (fun (isHierarchic, arc) -> treeFromArc isHierarchic arc)

[<EntryPoint>]
let main (args: string []) =
    // TODO
    (*
    let scheme_file = System.IO.Path.Combine (GameRes.FormatCatalog.Instance.DataDirectory, "Formats.dat")
    use file = System.IO.File.OpenRead (scheme_file)
    GameRes.FormatCatalog.Instance.DeserializeScheme (file)
    *)
    NineP.Server.listenAndServe args.[0] (fun _ -> loadArchive)
