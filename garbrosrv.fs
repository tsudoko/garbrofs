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

type File(arc: GameRes.ArcFile, entry: GameRes.Entry, ?name: string) =
    let entrySize =
        match entry with
        | :? GameRes.PackedEntry as entry -> entry.UnpackedSize
        | _ -> entry.Size

    let stat = Stat(
        qid = { Type = FileType.File; Ver = 0u; Path = pathCounter.PostAndReply(fun chan -> chan) },
        mode = 0o444u,
        atime = 0u,
        mtime = 0u,
        length = uint64 entrySize,
        name = defaultArg name entry.Name)

    interface IFile with
        member f.Stat =
            stat

        member f.Open(mode) =
            // TODO: handle automatically in NineP.Server based on stat.Mode
            if mode.HasFlag(OpenMode.Write) || mode.HasFlag(OpenMode.Rdwr) then
                Error "read only file system"
            else
                Ok (arc.OpenSeekableEntry(entry))

type Directory (stat: Stat, entries: Map<string, Node>) =
    new(entries: Map<string, Node>, name: string) =
        Directory(Stat(
            qid = { Type = FileType.Dir; Ver = 0u; Path = pathCounter.PostAndReply(fun chan -> chan) },
            mode = (0o555u ||| ((uint32 FileType.Dir) <<< 24)),
            atime = 0u,
            mtime = 0u,
            name = name), entries)

    member d.addEntry (e: Node) =
        Directory(stat, entries.Add(e.Stat.Name, e))

    // TODO: make default?
    member d.mapEntries =
        entries

    interface IDirectory with
        member f.Stat =
            stat

        member d.Entries =
            entries
            |> Map.toSeq
            |> Seq.map (fun (_, v) -> v)

let private pathSeparators = [|'/'; '\\'|]

let private treeFromArc (isHierarchic: bool) (arc: GameRes.ArcFile) =
    match isHierarchic with
    | true -> "hierarchical"
    | false -> "flat"
    |> printfn "%s"
    match isHierarchic with
    | true ->
        arc.Dir
        |> Seq.fold (fun (tree: Directory) entry ->
            let s = entry.Name.Split(pathSeparators, StringSplitOptions.RemoveEmptyEntries)
            s.AsSpan(0, s.Length-1).ToArray() // FIXME: unneeded copy
            |> Array.fold (fun (parent: Directory :: hier) pathElem ->
                parent.mapEntries
                |> Map.tryFind pathElem
                // this is dumb
                |> Option.bind (fun e ->
                    match e with
                    | Directory dd ->
                        match dd with
                        | :? Directory as d -> Some d
                        | _ -> None
                    | _ -> None)
                |> Option.defaultWith (fun () -> Directory(Map.empty, pathElem))
                |> fun x -> x :: parent :: hier
            ) (List.singleton tree)
            |> fun (d :: ds) -> d.addEntry(File(arc, entry, s |> Array.last) :> IFile |> Node.File) :: ds
            |> List.reduce (fun child parent -> parent.addEntry(child :> IDirectory |> Node.Directory))
        ) (Directory(Map.empty, "/"))
    | false ->
        arc.Dir
        |> Seq.map (fun entry -> entry.Name, File(arc, entry) :> IFile |> Node.File)
        |> Map.ofSeq
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
