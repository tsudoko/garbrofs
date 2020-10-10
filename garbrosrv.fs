module GARbroFS
open System
open System.IO
open System.Collections.Generic
open System.Collections.Immutable

open NineP
open NineP.Server
open NineP.ServerUtil

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

type Directory (stat: Stat, entries: IReadOnlyDictionary<string, Node>) =
    new(entries: IReadOnlyDictionary<string, Node>, name: string, qidPath: uint64) =
        Directory(
            Stat(qid = { Type = FileType.Dir; Ver = 0u; Path = qidPath },
                mode = (0o555u ||| ((uint32 FileType.Dir) <<< 24)),
                atime = 0u,
                mtime = 0u,
                name = name),
            entries)

    interface IDirectory with
        member f.Stat =
            stat

        member d.Entries =
            entries

let private treeFromArc (isHierarchic: bool) (arc: GameRes.ArcFile) =
    match isHierarchic with
    | true -> "hierarchical"
    | false -> "flat"
    |> printfn "%s"
    match isHierarchic with
    | true ->
        arc.Dir
        |> deflatten
            (fun name entry qidPath -> File(arc, entry, qidPath, name) :> IFile |> Node.File)
            (fun name entries qidPath -> Directory(entries, name, (1UL<<<63)+qidPath) :> IDirectory |> Node.Directory)
            (fun entry -> entry.Name)
            [|'/'; '\\'|]
        |> fun entries -> Directory(entries, "/", UInt64.MaxValue)
    | false ->
        arc.Dir
        |> Seq.indexed
        |> Seq.map (fun (i, entry) -> entry.Name, File(arc, entry, uint64 i) :> IFile |> Node.File)
        |> Map.ofSeq
        :> IReadOnlyDictionary<_, _>
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
        GameRes.FormatCatalog.Instance.LookupSignature<GameRes.ArchiveFormat>(signature)
        |> Seq.append (GameRes.FormatCatalog.Instance.LookupExtension<GameRes.ArchiveFormat>(Path.GetExtension(path).TrimStart('.')))
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
