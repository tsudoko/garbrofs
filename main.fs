module Main
open System
open System.Collections.Generic
open System.Net
open System.Net.Sockets

open NineP

type State = System.IO.Stream option

let mutable _msize: uint32 = 0u
let mutable qids: Map<uint32, Qid * State> = Map.empty
// TODO: move root to separate var, make paths immutable? archives don't change when they're mounted
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

let handle msg =
    match msg with
    | Tversion (msize, version) ->
        // TODO: reset state
        printfn "got Tversion %d %s" msize version
        if version.StartsWith("9P2000") then
            _msize <- msize
            Rversion (_msize, "9P2000")
        else
            Rversion (_msize, "unknown")
    | Tauth (_, _, _) ->
        Rerror "no authentication required"
    | Tattach (fid, afid, uname, aname) ->
        printfn "got Tattach %d %d %s %s" fid afid uname aname
        // TODO: return error if fid in use
        qids <- qids.Add(fid, (paths.[0].Qid, None))
        Rattach (paths.[0].Qid)
    | Twalk (fid, newfid, wnames) ->
        printfn "got Twalk %A %A %A" fid newfid wnames
        // TODO: handle .. in root dir
        if wnames.Length = 0 then
            if newfid <> fid then
                // TODO: return error if newfid in use (unless newfid = fid) or fid not found or fid open
                let (qid, _) = qids.[fid]
                qids <- qids.Add(newfid, (qid, None))
            Rwalk [||]
        else
            Rerror "Twalk on random files unimplemented"
    | Topen (fid, mode) ->
        printfn "got Topen %d %A" fid mode
        if mode.HasFlag(OpenMode.Write) || mode.HasFlag(OpenMode.Rdwr) then
            Rerror "read only file system"
        else
            match qids.TryFind(fid) with
            | Some (qid, _) ->
                qids <- qids.Add(fid, (qid, Some (upcast (new System.IO.MemoryStream("awoo"B)))))
                Ropen (qid, 0u)
            | None ->
                Rerror "fid unknown or out of range"
    | Tread (fid, offset, count) ->
        printfn "got Tread %d %d %d" fid offset count
        match qids.TryFind(fid) with
        | Some (qid, ms) ->
            match qid.Type.HasFlag(FileType.Dir) with
            | true -> Rread [||] // empty directory
            | false ->
                match ms with
                | Some s ->
                    // XXX: return error on overflow
                    if s.Position <> Checked.int64 offset then
                        s.Position <- Checked.int64 offset
                    let buf = Array.zeroCreate (int count)
                    let nread = s.Read(buf, 0, (int count))
                    Rread (buf.AsSpan(0, nread).ToArray()) // XXX unneeded copy
                | None ->
                    Rerror "file not open"
        | None ->
            Rerror "fid unknown or out of range"
    | Tclunk fid ->
        printfn "got Tclunk %d" fid
        let (_, ms) = qids.[fid]
        ms |> Option.map (fun s -> s.Dispose())
        qids <- qids.Remove(fid)
        Rclunk
    | Tstat fid ->
        printfn "got Tstat %d" fid
        let (qid, _) = qids.[fid]
        Rstat paths.[int qid.Path]
        // TODO: handle missing and invalid paths (from either qids or paths)
        //Rerror "Tstat unimplemented"
    | x ->
        Rerror <| sprintf "%A unimplemented" x


let rec listen (listener: TcpListener) =
    let client = listener.AcceptTcpClient()
    let stream = client.GetStream()
    NineP.P2000.serve stream handle // TODO: don't block
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
