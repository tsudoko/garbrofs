module Main
open System
open System.Collections.Generic
open System.Net
open System.Net.Sockets

open NineP

// TODO: move root to separate var, make paths immutable? archives don't change when they're mounted
// archives don't change, but it's generally impractical to pre-compute all paths if the server acts as an overlay as most files aren't ever going to be accessed unless someone runs du or something
// even more so if it's an overlay over a remote location
let mutable paths: Stat list = [Stat(
    type_ = 0us,
    dev = 0u,
    qid = { Type = FileType.Dir; Ver = 0u; Path = 0UL },
    mode = (0o555u ||| ((uint32 FileType.Dir) <<< 24)),
    atime = 0u,
    mtime = 0u,
    length = 0UL,
    name = "/",
    uid = "nobody",
    gid = "nobody",
    muid = "nobody"
)]

type FidState = System.IO.Stream option
type Session = { Msize: uint32; Fids: Map<uint32, Qid * FidState> }
let EmptySession = { Msize = 8192u; Fids = Map.empty }

type State =
    { Reader: NinePReader
      Writer: NinePWriter
      Session: Session } with
    static member create(s) =
        { Reader = new NinePReader(s)
          Writer = new NinePWriter(s)
          Session = EmptySession }
    // XXX: not sure if this makes sense as the record itself is immutable
    interface IDisposable with
        member s.Dispose() =
            s.Reader.Dispose()
            s.Writer.Dispose()

let handle session tag msg =
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
        // TODO: return error if fid in use
        { session with Fids = session.Fids.Add(fid, (paths.[0].Qid, None)) }, Rattach (paths.[0].Qid)
    | Twalk (fid, newfid, wnames) ->
        printfn "got Twalk %A %A %A" fid newfid wnames
        // TODO: handle .. in root dir
        if wnames.Length = 0 then
            let ns =
                if newfid <> fid then
                    // TODO: return error if newfid in use (unless newfid = fid) or fid not found or fid open
                    let (qid, _) = session.Fids.[fid]
                    { session with Fids = session.Fids.Add(newfid, (qid, None)) }
                else
                    session
            ns, Rwalk [||]
        else
            session, Rerror "Twalk on random files unimplemented"
    | Topen (fid, mode) ->
        printfn "got Topen %d %A" fid mode
        if mode.HasFlag(OpenMode.Write) || mode.HasFlag(OpenMode.Rdwr) then
            session, Rerror "read only file system"
        else
            match session.Fids.TryFind(fid) with
            | None ->
                session, Rerror "fid unknown or out of range"
            | Some (qid, _) ->
                { session with Fids = session.Fids.Add(fid, (qid, Some (upcast (new System.IO.MemoryStream("awoo"B))))) }, Ropen (qid, 0u)
    | Tread (fid, offset, count) ->
        printfn "got Tread %d %d %d" fid offset count
        match session.Fids.TryFind(fid) with
        | None ->
            session, Rerror "fid unknown or out of range"
        | Some (qid, ms) ->
            match qid.Type.HasFlag(FileType.Dir) with
            | true -> session, Rread [||] // empty directory
            | false ->
                match ms with
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
        let (_, ms) = session.Fids.[fid]
        ms |> Option.map (fun s -> s.Dispose())
        { session with Fids = session.Fids.Remove(fid) }, Rclunk
    | Tstat fid ->
        printfn "got Tstat %d" fid
        let (qid, _) = session.Fids.[fid]
        session, Rstat paths.[int qid.Path]
        // TODO: handle missing and invalid paths (from either qids or paths)
        //session, Rerror "Tstat unimplemented"
    | x ->
        session, Rerror <| sprintf "%A unimplemented" x

let rec serve state =
    let r =
        P2000.tryReadMsg state.Reader state.Session.Msize
        |> Result.bind (fun (tag, tmsg) ->
            let nsession, rmsg = handle state.Session tag tmsg
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

let rec listen (listener: TcpListener) =
    let client = listener.AcceptTcpClient()
    client.GetStream()
    |> State.create
    |> serve // TODO: don't block
    listen listener

[<EntryPoint>]
let main (args: string []) =
    if args.Length < 2 then
        eprintfn "usage: %s addr port" (System.Environment.GetCommandLineArgs().[0])
        1
    else

    let addr = IPAddress.Parse(args.[0])
    let port = Int32.Parse(args.[1])
    let listener = new TcpListener(addr, port)
    listener.Start()
    printfn "listening @ %s:%s" args.[0] args.[1]
    listen listener
