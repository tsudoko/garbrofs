module GARbroFS
open System
open System.Collections.Generic
open System.Collections.Immutable

open NineP
open NineP.Server

type File(arc: GameRes.ArcFile, entry: GameRes.Entry, qidPath: uint64, ?name: string) =
    let entrySize =
        match entry with
        | :? GameRes.PackedEntry as entry -> entry.UnpackedSize
        | _ -> entry.Size

    let stat =
        Stat(qid = { Type = FileType.File; Ver = 0u; Path = qidPath },
            mode = 0o444u,
            atime = 0u,
            mtime = 0u,
            length = uint64 entrySize,
            name = defaultArg name entry.Name)

    interface IFile with
        member f.Stat =
            stat

        member f.Open() =
            Ok (arc.OpenSeekableEntry(entry))

type Directory (stat: Stat, entries: Map<string, Node>) =
    new(entries: Map<string, Node>, name: string, qidPath: uint64) =
        Directory(
            Stat(qid = { Type = FileType.Dir; Ver = 0u; Path = qidPath },
                mode = (0o555u ||| ((uint32 FileType.Dir) <<< 24)),
                atime = 0u,
                mtime = 0u,
                name = name),
            entries)

    member d.addEntry (e: Node) =
        Directory(stat, entries.Add(e.Stat.Name, e))

    member d.mapEntries =
        entries

    interface IDirectory with
        member f.Stat =
            stat

        member d.Entries =
            upcast entries

let private pathSeparators = [|'/'; '\\'|]

module TreeState =
    let map f (x, nextDirId) =
        (f x, nextDirId)
    let getTree (x, nextDirId) =
        x

let deflatten<'a> (makeFile: string -> 'a -> uint64 -> File)
                  //(makeDirectory: string -> seq<Node> -> uint64 -> Directory)
                  (makeDirectory: string -> Map<string, Node> -> uint64 -> Directory)
                  (getPath: 'a -> string)
                  (pathSeparators: char [])
                  (entries: seq<'a>) =
    entries
    |> Seq.indexed
    |> Seq.fold (fun (tree: Directory, nextDirId) (i, entry) ->
        let s = (getPath entry).Split(pathSeparators, StringSplitOptions.RemoveEmptyEntries)
        s.AsSpan(0, s.Length-1).ToArray() // FIXME: unneeded copy
        |> Array.fold (fun (parent: Directory :: hier, nextDirId) pathElem ->
            parent.mapEntries
            |> Map.tryFind pathElem
            // this is dumb
            |> Option.bind (fun e ->
                match e with
                | Directory dd ->
                    match dd with
                    | :? Directory as d -> Some (d, nextDirId)
                    | _ -> None
                | _ -> None)
            |> Option.defaultWith (fun () -> (makeDirectory pathElem Map.empty nextDirId), nextDirId+1UL)
            |> TreeState.map (fun x -> x :: parent :: hier)
        ) (List.singleton tree, nextDirId)
        |> TreeState.map (fun (d :: ds) -> d.addEntry(makeFile (s |> Array.last) entry (uint64 i) :> IFile |> Node.File) :: ds)
        |> (TreeState.map << List.reduce) (fun child parent -> parent.addEntry(child :> IDirectory |> Node.Directory))
    ) ((makeDirectory "/" Map.empty UInt64.MaxValue), 0UL)
    |> TreeState.getTree

let private treeFromArc (isHierarchic: bool) (arc: GameRes.ArcFile) =
    match isHierarchic with
    | true -> "hierarchical"
    | false -> "flat"
    |> printfn "%s"
    match isHierarchic with
    | true ->
        arc.Dir
        |> deflatten
            (fun name entry qidPath -> File(arc, entry, qidPath, name))
            (fun name entries qidPath -> Directory(entries, name, (1UL<<<63)+qidPath))
            (fun entry -> entry.Name)
            pathSeparators
    | false ->
        arc.Dir
        |> Seq.indexed
        |> Seq.map (fun (i, entry) -> entry.Name, File(arc, entry, uint64 i) :> IFile |> Node.File)
        |> Map.ofSeq
        |> fun entries -> Directory(entries, "/", UInt64.MaxValue)
    :> IDirectory
    |> Node.Directory

let loadArchive path =
    // this mess allows all matching handlers to attempt to open the file while
    // also ensuring errors are not lost and properly reported
    let openArc (arcview: GameRes.ArcView) (impls: seq<GameRes.ArchiveFormat>) =
        let rec openArc' errors (arcview: GameRes.ArcView) (impls: seq<GameRes.ArchiveFormat>) =
            match impls |> Seq.tryHead with
            | Some impl ->
                try
                    match impl.TryOpen arcview with
                    | null -> openArc' errors arcview (impls |> Seq.tail)
                    | arc -> impl.IsHierarchic, arc
                with e ->
                    let emsg = sprintf "%A: %s" impl e.Message
                    openArc' (emsg :: errors) arcview (impls |> Seq.tail)
            | None ->
                match errors with
                | [] -> failwithf "no handlers found for %s" path
                | x ->
                    (sprintf "no handlers found for %s, some handlers returned errors:" path) :: x
                    :> seq<_>
                    |> String.concat "\n"
                    |> failwith
        openArc' List.empty arcview impls

    // GameRes.ArcFile.TryOpen uses VFS (global mutable state), can't use it here
    try
        let av = new GameRes.ArcView(path)
        let signature = av.View.ReadUInt32(0L)
        // TODO: do extensions too? there might be formats without signatures
        GameRes.FormatCatalog.Instance.LookupSignature<GameRes.ArchiveFormat>(signature)
        |> openArc av
        ||> treeFromArc
        |> Ok
    with e -> Error e.Message

[<EntryPoint>]
let main (args: string []) =
    // TODO
    (*
    let scheme_file = System.IO.Path.Combine (GameRes.FormatCatalog.Instance.DataDirectory, "Formats.dat")
    use file = System.IO.File.OpenRead (scheme_file)
    GameRes.FormatCatalog.Instance.DeserializeScheme (file)
    *)
    NineP.Server.listenAndServe args.[0] (fun _ -> loadArchive)
