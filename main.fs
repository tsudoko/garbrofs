module Main
open System
open System.Collections.Generic
open System.Net
open System.Net.Sockets

open NineP

type State =
    { Offset: uint64 }

type Srv() =
    let mutable _msize: uint32 = 0u
    let mutable qids: Map<uint32, Qid * State> = Map.empty
    // TODO: move root to separate var, make paths immutable? archives don't change when they're mounted
    let mutable paths: Stat list = [Stat(
        type_ = 0us,
        dev = 0u,
        qid = { Type = (uint8 FileType.Dir); Ver = 0u; Path = 0UL },
        mode = (0o555u ||| ((uint32 FileType.Dir) <<< 24)),
        atime = 0u,
        mtime = 0u,
        length = 0UL,
        name = "/",
        uid = "nobody",
        gid = "nobody",
        muid = "nobody"
    )]
    let EmptyState: State = { Offset = 0UL }

    interface IServer with
        member s.Tversion(msize: uint32) (v: string) =
            // TODO: reset state
            printfn "got Tversion %d %s" msize v
            if v.StartsWith("9P2000") then
                _msize <- msize
                Ok (_msize, "9P2000")
            else
                Ok (_msize, "unknown")
        member s.Tauth(afid: uint32) (uname: string) (aname: string) =
            Error "no authentication required"
        member s.Tattach(fid: uint32) (afid: uint32) (uname: string) (aname: string) =
            printfn "got Tattach %d %d %s %s" fid afid uname aname
            // TODO: return error if fid in use
            qids <- qids.Add(fid, (paths.[0].Qid(), EmptyState))
            Ok (paths.[0].Qid())
        member s.Twalk(fid: uint32) (newfid: uint32) (wnames: string []) =
            printfn "got Twalk %A %A %A" fid newfid wnames
            // TODO: handle .. in root dir
            if wnames.Length = 0 then
                if newfid <> fid then
                    // TODO: return error if newfid in use or fid not found
                    let (qid, _) = qids.[fid]
                    qids <- qids.Add(newfid, (qid, EmptyState))
                Ok [||]
            else
                Error "Twalk on random files unimplemented"
        member s.Topen (fid: uint32) (mode: OpenMode) =
            printfn "got Topen %d %A" fid mode
            if mode.HasFlag(OpenMode.Write) || mode.HasFlag(OpenMode.Rdwr) then
                Error "read only file system"
            else
                try
                    let (qid, _) = qids.[fid]
                    Ok (qid, 0u)
                with
                | :? System.ArgumentException ->
                    Error "fid unknown or out of range"
        member s.Tread (fid: uint32) (offset: uint64) (count: uint32) =
            printfn "got Tread %d %d %d" fid offset count
            try
                let (qid, _) = qids.[fid]
                Ok [||] // FIXME: all dirs/files empty for now
            with
            | :? System.ArgumentException ->
                Error "fid unknown or out of range"
        member s.Tclunk(fid: uint32) =
            printfn "got Tclunk %d" fid
            qids <- qids.Remove(fid)
            Ok ()
        member s.Tstat(fid: uint32) =
            printfn "got Tstat %d" fid
            let (qid, _) = qids.[fid]
            Ok paths.[int qid.Path]
            // TODO: handle missing and invalid paths (from either qids or paths)
            //Error "Tstat unimplemented"


let rec listen (listener: TcpListener) srv =
    let client = listener.AcceptTcpClient()
    let stream = client.GetStream()
    NineP.P2000.handle stream srv // TODO: don't block
    listen listener srv

[<EntryPoint>]
let main (args: string []) =
    if args.Length < 2 then
        eprintfn "usage: %s addr port" (System.Environment.GetCommandLineArgs().[0])
        1
    else

    let addr = IPAddress.Parse(args.[0])
    let port = Int32.Parse(args.[1])
    let listener = new TcpListener(addr, port)
    let srv = new Srv()
    listener.Start()
    printfn "listening @ %s:%s" args.[0] args.[1]
    listen listener srv
