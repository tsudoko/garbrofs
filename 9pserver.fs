module NineP.Server
open System
open System.IO
open System.Collections.Generic
open System.Net
open System.Net.Sockets

open NineP
open Util

let mutable chatty = false

let BufMsize = 8192u

type IDirectory =
    abstract member Stat: Stat
    abstract member Entries: IReadOnlyDictionary<string, Node>
and IFile =
    abstract member Stat: Stat
    abstract member Open: unit -> Result<System.IO.Stream, string>
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
    | SDirectory of IDirectory * Stat list option
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
    member n.IsOpen =
        match n with
        | SFile (_, Some _) | SDirectory (_, Some _) -> true
        | _ -> false
    static member fromNode n =
        match n with
        | File f -> SFile (f, None)
        | Directory d -> SDirectory (d, None)

type Session = { Msize: uint32; Fids: Map<uint32, NodeWithState> }
let EmptySession = { Msize = BufMsize; Fids = Map.empty }

type State =
    { ClientStream: Stream
      Buf: MemoryStream
      BufReader: NinePReader
      BufWriter: NinePWriter
      AttachHandler: string -> string -> Result<Node, string>
      Session: Session } with
    static member create (ah) (s) =
        let buf = new MemoryStream(Checked.int32 BufMsize |> Array.zeroCreate, 0, Checked.int32 BufMsize, true, true)
        { ClientStream = s
          Buf = buf
          BufReader = new NinePReader(buf)
          BufWriter = new NinePWriter(buf)
          AttachHandler = ah
          Session = EmptySession }
    // XXX: not sure if this makes sense as the record itself is immutable
    interface IDisposable with
        member s.Dispose() =
            s.BufReader.Dispose()
            s.BufWriter.Dispose()
            s.ClientStream.Dispose()

let handle attachHandler session tag msg =
    if chatty then
        eprintfn "got %A %d" msg tag
    match msg with
    | Tversion (msize, version) ->
        if version.StartsWith("9P2000") then
            // TODO: clamp msize when received msize > default msize
            { EmptySession with Msize = msize }, Rversion (msize, "9P2000")
        else
            EmptySession, Rversion (msize, "unknown")
    | Tauth _ ->
        session, Rerror Enoauth
    | Tattach (fid, afid, uname, aname) ->
        match attachHandler uname aname with
        | Ok root ->
            // TODO: close open files if any
            { session with Fids = session.Fids.Add(fid, NodeWithState.fromNode root) }, Rattach (root.Stat.Qid)
        | Error e ->
            session, Rerror e
    | Twalk (fid, newfid, wnames) ->
        // TODO: handle ..
        // TODO: handle .. in root dir

        // this is not very efficient
        let rec doWalk_ (root: Node) wnames (dirs: Node list) =
            match root with
            | File f -> dirs |> List.rev
            | Directory d ->
                match wnames |> Array.tryHead with
                | None -> dirs |> List.rev
                | Some n ->
                    match d.Entries.TryGetValue n with
                    | (true, e) -> doWalk_ e (wnames |> Array.tail) (e :: dirs)
                    | _ -> dirs |> List.rev
        let doWalk (root: Node) wnames =
            doWalk_ root wnames List.empty

        match session.Fids.TryFind(fid) with
        | None -> session, Rerror Enofid
        | Some e ->
            let dirs = wnames |> doWalk e.Node
            let newRoot =
                dirs
                |> List.tryLast
                |> Option.defaultValue e.Node
            if dirs = List.empty && wnames.Length <> 0 then
                session, Rerror Enotexist // FIXME: this breaks create attempts
            else if e.IsOpen then
                session, Rerror "walk of an open file"
            else if newfid <> fid && session.Fids.TryFind(newfid).IsSome then
                session, Rerror "new fid in use"
            else
                { session with Fids = session.Fids.Add(newfid, NodeWithState.fromNode newRoot) }, Rwalk (dirs |> List.map (fun d -> d.Stat.Qid) |> List.toArray)
    | Topen (fid, mode) ->
        // TODO: de-hardcode somehow, not sure if this is worth keeping at all though as you can just check file mode on a case-by-case basis
        if mode |> OpenMode.requiresWritePerm then
            session, Rerror Erdonly
        else
            match session.Fids.TryFind(fid) with
            | None ->
                session, Rerror Enofid
            | Some node  ->
                // TODO: either support multiple open streams here or disallow multiple open calls, we're going to leak streams otherwise
                match node with
                | SDirectory (d, _) ->
                    session, Ropen (d.Stat.Qid, 0u)
                | SFile (f, _) ->
                    // TODO: maybe care about user/group modes too
                    if mode |> OpenMode.requiresWritePerm && not (f.Stat.Mode &&& 2u = 2u) then
                        session, Rerror Eperm
                    else
                    match f.Open() with
                    | Error e -> session, Rerror e
                    | Ok stream -> { session with Fids = session.Fids.Add(fid, SFile (f, Some stream)) }, Ropen (f.Stat.Qid, 0u)
    | Tread (fid, offset, count) ->
        match session.Fids.TryFind(fid) with
        | None ->
            session, Rerror Enofid
        | Some node ->
            match node with
            | SDirectory (d, entries) ->
                let rec fittingEntries_ count (entries: Stat list) rentries =
                    match entries with
                    | s :: tail when count >= (uint32 s.Bytes.Length) -> fittingEntries_ (count-(uint32 s.Bytes.Length)) tail (s :: rentries)
                    | _ -> rentries |> List.map (fun s -> s.Bytes) :> byte [] seq |> Array.concat, entries
                let fittingEntries count (entries: Stat list) =
                    fittingEntries_ count entries List.empty

                let prepared, remaining =
                    entries
                    |> Option.orElse (d.Entries.Values |> Seq.map (fun n -> n.Stat) |> Seq.toList |> Some)
                    |> Option.map (fun e -> fittingEntries count e)
                    |> Option.get
                // maybe TODO: store length of sent responses and check if requested offset matches total sent length (the spec disallows non-sequential reads of directories)
                { session with Fids = session.Fids.Add(fid, SDirectory (d, Some remaining)) }, Rread (prepared.AsMemory() |> implicitConv)
            | SFile (f, stream) ->
                match stream with
                | None ->
                    session, Rerror Enotopen
                | Some s ->
                    let r =
                        try
                            let soffset = Checked.int64 offset
                            if s.Position <> soffset then
                                s.Position <- soffset
                            Ok ()
                        with
                        | :? OverflowException -> Error Es64off
                        | :? ArgumentOutOfRangeException as e
                            when (match s with :? MemoryStream -> true | _ -> false) ->
                                 Error "this file cannot be read past 2 GiB"
                    match r with
                    | Error e -> session, Rerror e
                    | _ ->
                        let buf = Array.zeroCreate (int count) // TODO: reuse?
                        try
                            // TODO: make async
                            let nread = s.Read(buf, 0, (int count))
                            session, Rread (buf.AsMemory(0, nread) |> implicitConv)
                        with
                        | :? IOException as e -> session, Rerror e.Message
    | Tclunk fid ->
        match session.Fids.TryFind(fid) with
        | None -> session, Rerror Enofid
        | Some node ->
            match node with
            | SFile (_, stream) -> stream |> Option.map (fun s -> s.Dispose())
            | _ -> None
            |> ignore
            { session with Fids = session.Fids.Remove(fid) }, Rclunk
    | Tstat fid ->
        match session.Fids.TryFind(fid) with
        | None -> session, Rerror Enofid
        | Some node -> session, Rstat node.Stat
    | x ->
        session, Rerror <| sprintf "%A unimplemented" x

let rec serve state = async {
    let readRaw (buffer: Memory<byte>) stream = async {
        let! nread = state.ClientStream.ReadAsync(buffer).AsTask() |> Async.AwaitTask
        if nread = 0 then
            raise (EndOfStreamException())
        else if nread < buffer.Length then
            failwith "incomplete read"
    }

    let writeRaw (bufStream: MemoryStream) stream = async {
        bufStream.SetLength bufStream.Position
        bufStream.Position <- 0L
        do! bufStream.CopyToAsync stream |> Async.AwaitTask
        bufStream.SetLength (int64 bufStream.Capacity)
        bufStream.Position <- 0L
    }

    let r = async {
        do! state.ClientStream |> readRaw (Memory<_>(state.Buf.GetBuffer(), 0, 4))
        let len = state.BufReader.ReadUInt32()
        // TODO: handle >2G messages? we'd need to do it in chunks as the
        //       default max .NET object size is ~2G
        if len >= (1u <<< 31) then
            failwith "message too large"
        let slen = int (len &&& uint32 Int32.MaxValue)
        do! state.ClientStream |> readRaw (Memory<_>(state.Buf.GetBuffer(), 4, slen - 4))
        state.Buf.Position <- 0L

        return P2000.readMsg state.BufReader state.Session.Msize
    }

    match! r |> Async.Catch with
    | Choice2Of2 e ->
        eprintfn "[%A] error: %s" state.ClientStream e.Message
        (state :> IDisposable).Dispose()
    | Choice1Of2 (tag, tmsg) ->
        let nsession, rmsg = handle state.AttachHandler state.Session tag tmsg
        let r = async {
            state.Buf.Position <- 0L
            P2000.writeMsg state.BufWriter tag rmsg
            do! state.ClientStream |> writeRaw state.Buf
        }

        match! r |> Async.Catch with
        | Choice2Of2 e ->
            eprintfn "[%A] error: %s" state.ClientStream e.Message
            (state :> IDisposable).Dispose()
        | Choice1Of2 () ->
            return! serve { state with Session = nsession }
}

let rec listenLoop (attachHandler: string -> string -> Result<Node, string>) (getClientStream: Async<System.IO.Stream>) = async {
    let! stream = getClientStream
    stream
    |> State.create attachHandler
    |> serve
    |> Async.Start
    return! listenLoop attachHandler getClientStream
}

let listen (spec: string): Async<System.IO.Stream> * string =
    let args = spec.Split('!')
    match args.[0] with
    | "tcp" ->
        let addr = IPAddress.Parse(args.[1])
        let port = Int32.Parse(args.[2])
        let listener = new TcpListener(addr, port)
        listener.Start()
        let actualAddr = listener.LocalEndpoint :?> IPEndPoint
        async {
            let! client = listener.AcceptTcpClientAsync() |> Async.AwaitTask
            return client.GetStream() :> _
        }, (sprintf "%O:%d" actualAddr.Address actualAddr.Port)
    | "netpipe" ->
        async {
            let pipe = new System.IO.Pipes.NamedPipeServerStream(args.[1])
            do! pipe.WaitForConnectionAsync() |> Async.AwaitTask
            return pipe :> _
        }, (sprintf "<%s>" spec) // https://github.com/dotnet/runtime/issues/28979
        // TODO: make sure the pipe is cleaned up after ^Cing
    | x -> sprintf "unsupported dial protocol: %s" args.[0] |> failwith

let listenAndServe (dialString: string) (attachHandler: string -> string -> Result<Node, string>) =
    listen dialString
    ||> fun x addr ->
        printfn "listening @ %s" addr
        x
    |> listenLoop attachHandler
    |> Async.RunSynchronously
