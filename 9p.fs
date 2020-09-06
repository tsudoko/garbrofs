namespace NineP

type NinePReader(args: System.IO.Stream) =
    inherit System.IO.BinaryReader(args)

    override r.ReadString() = (System.Text.Encoding.UTF8.GetString(r.ReadBytes(int32 (r.ReadUInt16()))))

type NinePWriter(args: System.IO.Stream) =
    inherit System.IO.BinaryWriter(args)

    override w.Write (s: string) =
        let bytes = System.Text.Encoding.UTF8.GetBytes(s)
        w.Write (uint16 bytes.Length)
        w.Write bytes

[<System.Flags>]
type OpenMode =
    | Read   = 0uy
    | Write  = 1uy
    | Rdwr   = 2uy
    | Exec   = 3uy
    | Trunc  = 0x10uy
    | Rclose = 0x40uy

type FileType =
    |  Dir    = 0b10000000uy
    |  Append = 0b01000000uy
    |  Excl   = 0b00100000uy
    |  Auth   = 0b00001000uy
    |  Tmp    = 0b00000100uy

type MsgType =
    | Tversion = 100uy
    | Rversion = 101uy
    | Tauth    = 102uy
    | Rauth    = 103uy
    | Tattach  = 104uy
    | Rattach  = 105uy
    | Rerror   = 107uy
    | Tflush   = 108uy
    | Rflush   = 109uy
    | Twalk    = 110uy
    | Rwalk    = 111uy
    | Topen    = 112uy
    | Ropen    = 113uy
    | Tcreate  = 114uy
    | Rcreate  = 115uy
    | Tread    = 116uy
    | Rread    = 117uy
    | Twrite   = 118uy
    | Rwrite   = 119uy
    | Tclunk   = 120uy
    | Rclunk   = 121uy
    | Tremove  = 122uy
    | Rremove  = 123uy
    | Tstat    = 124uy
    | Rstat    = 125uy
    | Twstat   = 126uy
    | Rwstat   = 127uy

type Qid =
    { Type: uint8
      Ver: uint32
      Path: uint64 }

type Stat =
    { Type: uint16
      Dev: uint32
      Qid: Qid
      Mode: uint32
      Atime: uint32
      Mtime: uint32
      Length: uint64
      Name: string
      Uid: string
      Gid: string
      Muid: string } with
        member s.Size (): uint16 =
            uint16 (
            sizeof<uint16> +
            sizeof<uint32> +
              sizeof<uint8> + // qid
              sizeof<uint32> +
              sizeof<uint64> +
            sizeof<uint32> +
            sizeof<uint32> +
            sizeof<uint32> +
            sizeof<uint64> +
            sizeof<uint16> +
            System.Text.Encoding.UTF8.GetByteCount(s.Name) +
            sizeof<uint16> +
            System.Text.Encoding.UTF8.GetByteCount(s.Uid) +
            sizeof<uint16> +
            System.Text.Encoding.UTF8.GetByteCount(s.Gid) +
            sizeof<uint16> +
            System.Text.Encoding.UTF8.GetByteCount(s.Muid))

        // FIXME: ugly
        member st.writeTo (w: NinePWriter) =
            w.Write (st.Size ())
            w.Write st.Type
            w.Write st.Dev
            w.Write st.Qid.Type
            w.Write st.Qid.Ver
            w.Write st.Qid.Path
            w.Write st.Mode
            w.Write st.Atime
            w.Write st.Mtime
            w.Write st.Length
            w.Write st.Name
            w.Write st.Uid
            w.Write st.Gid
            w.Write st.Muid

        // FIXME: ugly, unneeded allocations
        member s.GetBytes (): byte [] =
            let bytes = Array.create (int (s.Size()+2us)) 0uy
            use bv = new System.IO.MemoryStream(bytes)
            use w = new NinePWriter(bv)
            s.writeTo(w)
            bytes

type IServer =
    abstract Tversion: msize: uint32 -> v: string -> Result<uint32 * string, string>
    abstract Tauth: afid: uint32 -> uname: string -> aname: string -> Result<Qid, string>
    abstract Tattach: fid: uint32 -> afid: uint32 -> uname: string -> aname: string -> Result<Qid, string>
    abstract Twalk: fid: uint32 -> newfid: uint32 -> wnames: string [] -> Result<Qid [], string>
    abstract Topen: fid: uint32 -> mode: OpenMode -> Result<Qid * uint32, string>
    abstract Tread: fid: uint32 -> offset: uint64 -> count: uint32 -> Result<byte [], string>
    abstract Tclunk: fid: uint32 -> Result<unit, string>
    abstract Tstat: fid: uint32 -> Result<Stat, string>

module P2000 =
    open System.IO // Stream, BinaryReader
    open System.Text // Encoding

    [<Literal>]
    let NoTag = 65535us
    [<Literal>]
    let NoFid = 4294967295u
    [<Literal>]
    let MaxWelem = 16
    [<Literal>]
    let Ver = "9P2000"

    let rerror (w: NinePWriter) (tag: uint16) (err: string) =
        w.Write (uint32 (4+1+2+2+(Encoding.UTF8.GetByteCount err)))
        w.Write (uint8 MsgType.Rerror)
        w.Write tag
        w.Write err

    let rec handle_ (r: NinePReader) (w: NinePWriter) (srv: IServer) =
        let len = r.ReadUInt32()
        let mtype = Microsoft.FSharp.Core.LanguagePrimitives.EnumOfValue<uint8, MsgType>(r.ReadByte())
        let tag = r.ReadUInt16()
        printfn "mtype %d %A tag %d" (uint8 mtype) mtype tag

        match mtype with
        // TODO: look for a way to reduce match clauses here? the rerror case is always the same
        // TODO: calulate sizes automatially, maybe find a way to write complex types directly
        // TODO: check if msg size doesn't exceed srv.msize on every send
        | MsgType.Tversion ->
            match srv.Tversion (r.ReadUInt32()) (r.ReadString()) with
            | Ok (msize, v) ->
                w.Write (uint32 (4+1+2+4+2+Encoding.UTF8.GetByteCount(v)))
                w.Write (uint8 MsgType.Rversion)
                w.Write tag
                w.Write msize
                w.Write v
            | Error err -> rerror w tag err
        | MsgType.Tauth ->
            match srv.Tauth (r.ReadUInt32()) (r.ReadString()) (r.ReadString()) with
            | Ok q ->
                w.Write (uint32 (4+1+2+13))
                w.Write (uint8 MsgType.Rauth)
                w.Write tag
                w.Write q.Type
                w.Write q.Ver
                w.Write q.Path
            | Error err -> rerror w tag err
        | MsgType.Tattach ->
            match srv.Tattach (r.ReadUInt32()) (r.ReadUInt32()) (r.ReadString()) (r.ReadString()) with
            | Ok q ->
                w.Write (uint32 (4+1+2+13))
                w.Write (uint8 MsgType.Rattach)
                w.Write tag
                w.Write q.Type
                w.Write q.Ver
                w.Write q.Path
            | Error err -> rerror w tag err
        | MsgType.Twalk ->
            let fid = r.ReadUInt32()
            let newfid = r.ReadUInt32()
            let nwname = r.ReadUInt16()
            // TODO: check if wnames <= MaxWelem
            let mutable wnames = Array.create (int nwname) ""
            for i = 1 to wnames.Length do
                Array.set wnames (i-1) (r.ReadString())
            match srv.Twalk fid newfid wnames with
            // TODO: check if qs.Length <= MaxWelem
            | Ok qs ->
                w.Write (uint32 (4+1+2+2+13*qs.Length))
                w.Write (uint8 MsgType.Rwalk)
                w.Write tag
                w.Write (uint16 qs.Length)
                for q in qs do
                    w.Write q.Type
                    w.Write q.Ver
                    w.Write q.Path
            | Error err -> rerror w tag err
        | MsgType.Topen ->
            match srv.Topen (r.ReadUInt32()) (LanguagePrimitives.EnumOfValue<uint8, OpenMode>(r.ReadByte())) with
            | Ok (q, iounit) ->
                w.Write (uint32 (4+1+2+13+4))
                w.Write (uint8 MsgType.Ropen)
                w.Write tag
                w.Write q.Type
                w.Write q.Ver
                w.Write q.Path
                w.Write iounit
            | Error err -> rerror w tag err
        | MsgType.Tread ->
            match srv.Tread (r.ReadUInt32()) (r.ReadUInt64()) (r.ReadUInt32()) with
            | Ok data ->
                w.Write (uint32 (4+1+2+4+data.Length))
                w.Write (uint8 MsgType.Rread)
                w.Write tag
                w.Write (uint32 data.Length)
                w.Write data
            | Error err -> rerror w tag err
        | MsgType.Tclunk ->
            match srv.Tclunk (r.ReadUInt32()) with
            | Ok () ->
                w.Write (uint32 (4+1+2))
                w.Write (uint8 MsgType.Rclunk)
                w.Write tag
            | Error err -> rerror w tag err
        | MsgType.Tstat ->
            match srv.Tstat (r.ReadUInt32()) with
            | Ok st ->
                w.Write (4u+1u+2u+2u+2u+(uint32 (st.Size ())))
                w.Write (uint8 MsgType.Rstat)
                w.Write tag
                w.Write (st.Size ()+2us)
                st.writeTo w
            | Error err -> rerror w tag err
        | x ->
            ignore (r.ReadBytes (int32 (len-4u-1u-2u)))
            rerror w tag (sprintf "unimplemented message type: %d" (uint8 x))

        handle_ r w srv

    let handle (s: Stream) (srv: IServer) =
        use r = new NinePReader(s)
        use w = new NinePWriter(s)
        try
            handle_ r w srv
        with
        | :? System.IO.EndOfStreamException -> printfn "eof"

