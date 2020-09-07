namespace NineP

open System
open System.Text

open Util

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

type Stat(bytes: byte []) =
    let b = bytes

    let namestart = 41
    let namelen = int (LittleEndian.ru16 (ReadOnlySpan<byte>(b, namestart, 2)))
    let uidstart = int (namestart+2+namelen)
    let uidlen = int (LittleEndian.ru16 (ReadOnlySpan<byte>(b, uidstart, 2)))
    let gidstart = int (uidstart+2+uidlen)
    let gidlen = int (LittleEndian.ru16 (ReadOnlySpan<byte>(b, gidstart, 2)))
    let muidstart = int (gidstart+2+gidlen)
    let muidlen = int (LittleEndian.ru16 (ReadOnlySpan<byte>(b, muidstart, 2)))

    member st.Bytes = b
    member st.Size() = LittleEndian.ru16 (ReadOnlySpan<byte>(b, 0, 2))
    member st.Type() = LittleEndian.ru16 (ReadOnlySpan<byte>(b, 2, 2))
    member st.Dev() = LittleEndian.ru32 (ReadOnlySpan<byte>(b, 4, 4))
    member st.Qid(): Qid =
        { Type = b.[8]
          Ver = LittleEndian.ru32 (ReadOnlySpan<byte>(b, 9, 4))
          Path = LittleEndian.ru64 (ReadOnlySpan<byte>(b, 13, 8)) }
    member st.Mode() = LittleEndian.ru32 (ReadOnlySpan<byte>(b, 21, 4))
    member st.Atime() = LittleEndian.ru32 (ReadOnlySpan<byte>(b, 25, 4))
    member st.Mtime() = LittleEndian.ru32 (ReadOnlySpan<byte>(b, 29, 4))
    member st.Length() = LittleEndian.ru64 (ReadOnlySpan<byte>(b, 33, 8))
    member st.Name() = Encoding.UTF8.GetString(ReadOnlySpan<byte>(b, namestart+2, namelen))
    member st.Uid() = Encoding.UTF8.GetString(ReadOnlySpan<byte>(b, uidstart+2, uidlen))
    member st.Gid() = Encoding.UTF8.GetString(ReadOnlySpan<byte>(b, gidstart+2, gidlen))
    member st.Muid() = Encoding.UTF8.GetString(ReadOnlySpan<byte>(b, muidstart+2, muidlen))

    new(type_: uint16, dev: uint32, qid: Qid, mode: uint32, atime: uint32, mtime: uint32, length: uint64, name: string, uid: string, gid: string, muid: string) =
        let namelen = Encoding.UTF8.GetByteCount(name)
        let uidlen = Encoding.UTF8.GetByteCount(uid)
        let gidlen = Encoding.UTF8.GetByteCount(gid)
        let muidlen = Encoding.UTF8.GetByteCount(muid)

        let size = 41+2+namelen+2+uidlen+2+gidlen+2+muidlen
        let uidstart = 41+2+namelen
        let gidstart = uidstart+2+uidlen
        let muidstart = gidstart+2+gidlen
        let b = Array.create size 0uy
        LittleEndian.wu16(uint16 size, b.AsSpan(0, 2))
        LittleEndian.wu16(type_, b.AsSpan(2, 2))
        LittleEndian.wu32(dev, b.AsSpan(4, 4))
        b.[8] <- qid.Type
        LittleEndian.wu32(qid.Ver, b.AsSpan(9, 4))
        LittleEndian.wu64(qid.Path, b.AsSpan(13, 8))
        LittleEndian.wu32(mode, b.AsSpan(21, 4))
        LittleEndian.wu32(atime, b.AsSpan(25, 4))
        LittleEndian.wu32(mtime, b.AsSpan(29, 4))
        LittleEndian.wu64(length, b.AsSpan(33, 8))
        LittleEndian.wu16(uint16 namelen, b.AsSpan(41, 2))
        Encoding.UTF8.GetBytes(name.AsSpan(), b.AsSpan(43, namelen)) |> ignore
        LittleEndian.wu16(uint16 uidlen, b.AsSpan(uidstart, 2))
        Encoding.UTF8.GetBytes(uid.AsSpan(), b.AsSpan(uidstart+2, uidlen)) |> ignore
        LittleEndian.wu16(uint16 gidlen, b.AsSpan(gidstart, 2))
        Encoding.UTF8.GetBytes(gid.AsSpan(), b.AsSpan(gidstart+2, gidlen)) |> ignore
        LittleEndian.wu16(uint16 muidlen, b.AsSpan(muidstart, 2))
        Encoding.UTF8.GetBytes(muid.AsSpan(), b.AsSpan(muidstart+2, muidlen)) |> ignore
        Stat b

type Tmsg =
    | Tversion of msize: uint32 * version: string
    | Tauth of afid: uint32 * uname: string * aname: string
    | Tattach of fid: uint32 * afid: uint32 * uname: string * aname: string
    | Twalk of fid: uint32 * newfid: uint32 * wnames: string []
    | Topen of fid: uint32 * mode: OpenMode
    | Tread of fid: uint32 * offset: uint64 * count: uint32
    | Tclunk of fid: uint32
    | Tstat of fid: uint32
    | Tunknown of mtype: MsgType

type Rmsg =
    | Rversion of msize: uint32 * version: string
    | Rauth of aqid: Qid
    | Rattach of qid: Qid
    | Rerror of ename: string
    | Rwalk of nwqids: Qid []
    | Ropen of qid: Qid * iounit: uint32
    | Rread of data: byte []
    | Rclunk
    | Rstat of stat: Stat

type Msg =
    | Tmsg
    | Rmsg

type NinePReader(args: System.IO.Stream) =
    inherit System.IO.BinaryReader(args)

    override r.ReadString() = (System.Text.Encoding.UTF8.GetString(r.ReadBytes(int32 (r.ReadUInt16()))))

type NinePWriter(args: System.IO.Stream) =
    inherit System.IO.BinaryWriter(args)

    override w.Write (s: string) =
        let bytes = System.Text.Encoding.UTF8.GetBytes(s)
        w.Write (uint16 bytes.Length)
        w.Write bytes

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

    let rec serve_ (r: NinePReader) (w: NinePWriter) (handle: Tmsg -> Rmsg) =
        let len = r.ReadUInt32()
        let mtype = Microsoft.FSharp.Core.LanguagePrimitives.EnumOfValue<uint8, MsgType>(r.ReadByte())
        let tag = r.ReadUInt16()
        printfn "mtype %d %A tag %d" (uint8 mtype) mtype tag

        let msg: Tmsg =
            match mtype with
            | MsgType.Tversion -> Tversion(r.ReadUInt32(), r.ReadString())
            | MsgType.Tauth -> Tauth(r.ReadUInt32(), r.ReadString(), r.ReadString())
            | MsgType.Tattach -> Tattach(r.ReadUInt32(), r.ReadUInt32(), r.ReadString(), r.ReadString())
            // TODO: check if wnames <= MaxWelem
            | MsgType.Twalk -> Twalk(r.ReadUInt32(), r.ReadUInt32(), Array.init (int (r.ReadUInt16())) (fun _ -> r.ReadString()))
            | MsgType.Topen -> Topen(r.ReadUInt32(), LanguagePrimitives.EnumOfValue (r.ReadByte()))
            | MsgType.Tread -> Tread(r.ReadUInt32(), r.ReadUInt64(), r.ReadUInt32())
            | MsgType.Tclunk -> Tclunk(r.ReadUInt32())
            | MsgType.Tstat -> Tstat(r.ReadUInt32())
            | x ->
                ignore <| r.ReadBytes (int32 <| len-4u-1u-2u)
                Tunknown x

        // TODO: calulate sizes automatially, maybe find a way to write complex types directly
        // TODO: check if msg size doesn't exceed srv.msize on every send
        match handle msg with
        | Rversion (msize, v) ->
            w.Write (uint32 (4+1+2+4+2+Encoding.UTF8.GetByteCount(v)))
            w.Write (uint8 MsgType.Rversion)
            w.Write tag
            w.Write msize
            w.Write v
        | Rauth aqid ->
            w.Write (uint32 (4+1+2+13))
            w.Write (uint8 MsgType.Rauth)
            w.Write tag
            w.Write aqid.Type
            w.Write aqid.Ver
            w.Write aqid.Path
        | Rattach qid ->
            w.Write (uint32 (4+1+2+13))
            w.Write (uint8 MsgType.Rattach)
            w.Write tag
            w.Write qid.Type
            w.Write qid.Ver
            w.Write qid.Path
        | Rerror ename ->
            w.Write (uint32 (4+1+2+2+(Encoding.UTF8.GetByteCount ename)))
            w.Write (uint8 MsgType.Rerror)
            w.Write tag
            w.Write ename
        | Rwalk qids ->
            // TODO: check if qids.Length <= MaxWelem
            w.Write (uint32 (4+1+2+2+13*qids.Length))
            w.Write (uint8 MsgType.Rwalk)
            w.Write tag
            w.Write (uint16 qids.Length)
            for q in qids do
                w.Write q.Type
                w.Write q.Ver
                w.Write q.Path
        | Ropen (qid, iounit) ->
            w.Write (uint32 (4+1+2+13+4))
            w.Write (uint8 MsgType.Ropen)
            w.Write tag
            w.Write qid.Type
            w.Write qid.Ver
            w.Write qid.Path
            w.Write iounit
        | Rread data ->
            w.Write (uint32 (4+1+2+4+data.Length))
            w.Write (uint8 MsgType.Rread)
            w.Write tag
            w.Write (uint32 data.Length)
            w.Write data
        | Rclunk ->
            w.Write (uint32 (4+1+2))
            w.Write (uint8 MsgType.Rclunk)
            w.Write tag
        | Rstat stat ->
            w.Write (4u+1u+2u+2u+(uint32 (stat.Bytes.Length)))
            w.Write (uint8 MsgType.Rstat)
            w.Write tag
            w.Write (uint16 stat.Bytes.Length)
            w.Write (stat.Bytes)

        serve_ r w handle

    let serve (s: Stream) handle =
        use r = new NinePReader(s)
        use w = new NinePWriter(s)
        try
            serve_ r w handle
        with
        | :? System.IO.EndOfStreamException -> printfn "eof"

