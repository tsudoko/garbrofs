module NineP.Server
open System
open System.Collections.Generic
open System.Net
open System.Net.Sockets

open NineP

type IDirectory =
    abstract member Stat: Stat
    abstract member Entries: Node seq
and IFile =
    abstract member Stat: Stat
    abstract member Open: OpenMode -> Result<System.IO.Stream, string>
and Node =
    | File of IFile
    | Directory of IDirectory
    // TODO: reduce boilerplate
    with
    member n.Stat =
        match n with
        | File f -> f.Stat
        | Directory d -> d.Stat

type NodeWithState =
    | SFile of IFile * System.IO.Stream option
    | SDirectory of IDirectory * Stat seq option
    // TODO: reduce boilerplate
    with
    member n.Stat =
        match n with
        | SFile (f, _) -> f.Stat
        | SDirectory (d, _) -> d.Stat
    member n.Node =
        match n with
        | SFile (f, _) -> File f
        | SDirectory (d, _) -> Directory d
    static member fromNode n =
        match n with
        | File f -> SFile (f, None)
        | Directory d -> SDirectory (d, None)

type Session = { Msize: uint32; Fids: Map<uint32, NodeWithState> }
let EmptySession = { Msize = 8192u; Fids = Map.empty }

type State =
    { Reader: NinePReader
      Writer: NinePWriter
      AttachHandler: string -> string -> Result<Node, string>
      Session: Session } with
    static member create (ah) (s) =
        { Reader = new NinePReader(s)
          Writer = new NinePWriter(s)
          AttachHandler = ah
          Session = EmptySession }
    // XXX: not sure if this makes sense as the record itself is immutable
    interface IDisposable with
        member s.Dispose() =
            s.Reader.Dispose()
            s.Writer.Dispose()

type TestingDirectory(name: string, entries: Node seq) =
    interface IDirectory with
        member d.Stat =
            Stat(qid = { Type = FileType.Dir; Ver = 0u; Path = 0UL },
                mode = (0o555u ||| ((uint32 FileType.Dir) <<< 24)),
                name = name)

        member d.Entries =
            entries

type TestingProxyFile(name: string, path: string) =
    interface IFile with
        member f.Stat =
            Stat(mode = 0o444u,
                length = uint64 (System.IO.FileInfo(path).Length),
                name = name)

        member f.Open mode =
            Ok (System.IO.File.OpenRead(path) :> System.IO.Stream)

type TestingFile(name: string, contents: byte []) =
    interface IFile with
        member f.Stat =
            Stat(mode = 0o444u,
                length = uint64 contents.Length,
                name = name)

        member f.Open mode =
            Ok (new System.IO.MemoryStream(contents) :> System.IO.Stream)

let handle attachHandler session tag msg =
    match msg with
    | Tversion (msize, version) ->
        printfn "got Tversion %d %s" msize version
        if version.StartsWith("9P2000") then
            // TODO: clamp msize when received msize > default msize? it's not like we care that much though as long as it's not something ridiculous
            { EmptySession with Msize = msize }, Rversion (msize, "9P2000")
        else
            EmptySession, Rversion (msize, "unknown")
    | Tauth _ ->
        session, Rerror "no authentication required"
    | Tattach (fid, afid, uname, aname) ->
        printfn "got Tattach %d %d %s %s" fid afid uname aname
        match attachHandler uname aname with
        | Ok root ->
            // TODO: close open files if any
            { session with Fids = session.Fids.Add(fid, NodeWithState.fromNode root) }, Rattach (root.Stat.Qid)
        | Error e ->
            session, Rerror e
    | Twalk (fid, newfid, wnames) ->
        printfn "got Twalk %A %A %A" fid newfid wnames
        // TODO: handle .. in root dir

        // this is not very efficient
        let rec doWalk_ (root: Node) wnames (dirs: Node list) =
            match root with
            | File f -> dirs |> List.rev
            | Directory d ->
                match wnames |> Array.tryHead with
                | None -> dirs |> List.rev
                | Some n ->
                    match d.Entries |> Seq.tryFind (fun e -> e.Stat.Name = n) with
                    | None -> dirs |> List.rev
                    | Some e -> doWalk_ e (wnames |> Array.tail) (e :: dirs)
        let doWalk (root: Node) wnames =
            doWalk_ root wnames List.empty

        match session.Fids.TryFind(fid) with
        | None -> session, Rerror "fid unknown or out of range"
        | Some e ->
            let dirs = wnames |> doWalk e.Node
            let newRoot =
                dirs
                |> List.tryLast
                |> Option.defaultValue e.Node
            match dirs = List.empty && wnames.Length <> 0 with
            | true -> session, Rerror "no such file or directory"
            | false ->
                // TODO: return error if newfid in use (unless newfid = fid) or fid open
                let session' =
                    match newfid <> fid with
                    | false -> session
                    | true ->
                        // TODO: reduce boilerplate
                        match newRoot with
                        | File f -> { session with Fids = session.Fids.Add(newfid, SFile (f, None)) }
                        | Directory d -> { session with Fids = session.Fids.Add(newfid, SDirectory (d, None)) }
                session', Rwalk (dirs |> List.map (fun d -> d.Stat.Qid) |> List.toArray)
    | Topen (fid, mode) ->
        printfn "got Topen %d %A" fid mode
        if mode.HasFlag(OpenMode.Write) || mode.HasFlag(OpenMode.Rdwr) then
            session, Rerror "read only file system"
        else
            match session.Fids.TryFind(fid) with
            | None ->
                session, Rerror "fid unknown or out of range"
            | Some node  ->
                // TODO: either support multiple open streams here or disallow multiple open calls, we're going to leak streams otherwise
                match node with
                | SDirectory (d, _) ->
                    session, Ropen (d.Stat.Qid, 0u)
                | SFile (f, _) ->
                    match f.Open(mode) with
                    | Error e -> session, Rerror e
                    | Ok stream -> { session with Fids = session.Fids.Add(fid, SFile (f, Some stream)) }, Ropen (f.Stat.Qid, 0u)
    | Tread (fid, offset, count) ->
        printfn "got Tread %d %d %d" fid offset count
        match session.Fids.TryFind(fid) with
        | None ->
            session, Rerror "fid unknown or out of range"
        | Some node ->
            match node with
            | SDirectory (d, entries) ->
                let rec fittingEntries_ count (entries: Stat seq) rentries =
                    match Seq.tryHead entries with
                    | Some s when count >= (uint32 s.Bytes.Length) -> fittingEntries_ (count-(uint32 s.Bytes.Length)) (entries |> Seq.tail) (s :: rentries)
                    | _ -> rentries |> List.map (fun s -> s.Bytes) :> byte [] seq |> Array.concat, entries
                let fittingEntries count (entries: Stat seq) =
                    fittingEntries_ count entries List.empty

                let prepared, remaining =
                    entries
                    |> Option.orElse (d.Entries |> Seq.map (fun n -> n.Stat) |> Some)
                    |> Option.map (fun e -> fittingEntries count e)
                    |> Option.get
                // maybe TODO: store length of sent responses and check if requested offset matches total sent length (the spec disallows non-sequential reads of directories)
                { session with Fids = session.Fids.Add(fid, SDirectory (d, Some remaining)) }, Rread prepared
            | SFile (f, stream) ->
                match stream with
                | None ->
                    session, Rerror "file not open"
                | Some s ->
                    // XXX: return error on overflow
                    if s.Position <> Checked.int64 offset then
                        s.Position <- Checked.int64 offset
                    let buf = Array.zeroCreate (int count)
                    let nread = s.Read(buf, 0, (int count))
                    session, Rread (buf.AsSpan(0, nread).ToArray()) // XXX unneeded copy
    | Tclunk fid ->
        printfn "got Tclunk %d" fid
        // TODO: handle invalid fid
        let node = session.Fids.[fid]
        match node with
        | SFile (_, stream) -> stream |> Option.map (fun s -> s.Dispose())
        | _ -> None
        { session with Fids = session.Fids.Remove(fid) }, Rclunk
    | Tstat fid ->
        printfn "got Tstat %d" fid
        let node = session.Fids.[fid]
        session, Rstat node.Stat
        // TODO: handle missing and invalid paths (from either qids or paths)
        //session, Rerror "Tstat unimplemented"
    | x ->
        session, Rerror <| sprintf "%A unimplemented" x

let rec serve state =
    let r =
        P2000.tryReadMsg state.Reader state.Session.Msize
        |> Result.bind (fun (tag, tmsg) ->
            let nsession, rmsg = handle state.AttachHandler state.Session tag tmsg
            P2000.tryWriteMsg state.Writer tag rmsg
            |> Result.map (fun _ -> nsession)
        )

    match r with
    | Ok nsession ->
        serve { state with Session = nsession }
    | Error e ->
        eprintfn "[%A] error: %s" state.Reader.BaseStream e.Message
        state.Reader.Dispose()
        state.Writer.Dispose()

let rec listen_ (attachHandler: string -> string -> Result<Node, string>) (getClientStream: unit -> System.IO.Stream) =
    getClientStream()
    |> State.create attachHandler
    |> serve // TODO: don't block
    listen_ attachHandler getClientStream

// TODO: move to another module to avoid name clashes
let listen (spec: string): unit -> System.IO.Stream =
    let args = spec.Split('!')
    match args.[0] with
    | "tcp" ->
        let addr = IPAddress.Parse(args.[1])
        let port = Int32.Parse(args.[2])
        let listener = new TcpListener(addr, port)
        listener.Start()
        fun () -> listener.AcceptTcpClient().GetStream() :> System.IO.Stream
    | x -> sprintf "unsupported dial protocol: %s" args.[0] |> failwith

let listenAndServe (dialString: string) (attachHandler: string -> string -> Result<Node, string>) =
    printfn "listening @ %s" dialString // TODO: don't print this until we actually create the listening socket
    listen dialString
    |> listen_ attachHandler

let main (args: string []) =
    if args.Length < 1 then
        eprintfn "usage: %s tcp!listenaddr!port" (System.Environment.GetCommandLineArgs().[0])
        1
    else

    let d4 = TestingDirectory("dir4", Seq.empty)
    let fa = TestingFile("a", "awoo"B)
    let fb = TestingFile("b", "AWOO"B)
    let fc = TestingProxyFile("c.fs", "9p.fs")
    let fd = TestingProxyFile("d.fs", "main.fs")
    let d3 = TestingDirectory("dir3", [Directory d4; File fd] :> Node seq)
    let d1 = TestingDirectory("dir1", [Directory d3; File fc] :> Node seq)
    let d2 = TestingDirectory("dir2", Seq.empty)
    let root = TestingDirectory("/", [Directory d1; Directory d2; File fa; File fb] :> Node seq)

    listenAndServe args.[0] (fun _ _ -> Ok (Directory root))
