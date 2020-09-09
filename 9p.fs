namespace NineP

open System
open System.Text
open System.Buffers.Binary

[<System.Flags>]
type OpenMode =
    | Read   = 0uy
    | Write  = 1uy
    | Rdwr   = 2uy
    | Exec   = 3uy
    | Trunc  = 0x10uy
    | Rclose = 0x40uy

[<System.Flags>]
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
    { Type: FileType
      Ver: uint32
      Path: uint64 }

type Stat(bytes: byte []) =
    let stringOffset = 41

    member inline st.AsSpan(from, len) = ReadOnlySpan<byte>(st.Bytes, from, len)

    member private st.nthString n =
        st.nthString_ n stringOffset
    member private st.nthString_ n offset =
        match n with
        | 0 ->
            Encoding.UTF8.GetString(st.AsSpan(offset+2, int (BinaryPrimitives.ReadUInt16LittleEndian(st.AsSpan(offset, 2)))))
        | x ->
            st.nthString_ (n-1) (offset+2+(int (BinaryPrimitives.ReadUInt16LittleEndian(st.AsSpan(offset, 2)))))

    member st.Bytes = bytes
    member st.Size = BinaryPrimitives.ReadUInt16LittleEndian(st.AsSpan(0, 2))
    member st.Type = BinaryPrimitives.ReadUInt16LittleEndian(st.AsSpan(2, 2))
    member st.Dev = BinaryPrimitives.ReadUInt32LittleEndian(st.AsSpan(4, 4))
    member st.Qid: Qid =
        { Type = LanguagePrimitives.EnumOfValue st.Bytes.[8]
          Ver = BinaryPrimitives.ReadUInt32LittleEndian(st.AsSpan(9, 4))
          Path = BinaryPrimitives.ReadUInt64LittleEndian(st.AsSpan(13, 8)) }
    member st.Mode = BinaryPrimitives.ReadUInt32LittleEndian(st.AsSpan(21, 4))
    member st.Atime = BinaryPrimitives.ReadUInt32LittleEndian(st.AsSpan(25, 4))
    member st.Mtime = BinaryPrimitives.ReadUInt32LittleEndian(st.AsSpan(29, 4))
    member st.Length = BinaryPrimitives.ReadUInt64LittleEndian(st.AsSpan(33, 8))
    member st.Name = st.nthString 0
    member st.Uid = st.nthString 1
    member st.Gid = st.nthString 2
    member st.Muid = st.nthString 3

    override st.ToString() =
        sprintf "'%s' '%s' '%s' '%s' q (%016x %d %A) m %012o at %d mt %d l %d t %d d %d" st.Name st.Uid st.Gid st.Muid st.Qid.Path st.Qid.Ver st.Qid.Type st.Mode st.Atime st.Mtime st.Length st.Type st.Dev

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
        BinaryPrimitives.WriteUInt16LittleEndian(b.AsSpan(0, 2), uint16 (size-2))
        BinaryPrimitives.WriteUInt16LittleEndian(b.AsSpan(2, 2), type_)
        BinaryPrimitives.WriteUInt32LittleEndian(b.AsSpan(4, 4), dev)
        b.[8] <- uint8 qid.Type
        BinaryPrimitives.WriteUInt32LittleEndian(b.AsSpan(9, 4), qid.Ver)
        BinaryPrimitives.WriteUInt64LittleEndian(b.AsSpan(13, 8), qid.Path)
        BinaryPrimitives.WriteUInt32LittleEndian(b.AsSpan(21, 4), mode)
        BinaryPrimitives.WriteUInt32LittleEndian(b.AsSpan(25, 4), atime)
        BinaryPrimitives.WriteUInt32LittleEndian(b.AsSpan(29, 4), mtime)
        BinaryPrimitives.WriteUInt64LittleEndian(b.AsSpan(33, 8), length)
        BinaryPrimitives.WriteUInt16LittleEndian(b.AsSpan(41, 2), uint16 namelen)
        Encoding.UTF8.GetBytes(name.AsSpan(), b.AsSpan(43, namelen)) |> ignore
        BinaryPrimitives.WriteUInt16LittleEndian(b.AsSpan(uidstart, 2), uint16 uidlen)
        Encoding.UTF8.GetBytes(uid.AsSpan(), b.AsSpan(uidstart+2, uidlen)) |> ignore
        BinaryPrimitives.WriteUInt16LittleEndian(b.AsSpan(gidstart, 2), uint16 gidlen)
        Encoding.UTF8.GetBytes(gid.AsSpan(), b.AsSpan(gidstart+2, gidlen)) |> ignore
        BinaryPrimitives.WriteUInt16LittleEndian(b.AsSpan(muidstart, 2), uint16 muidlen)
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

// NinePReader has a big flaw: ReadString allocates a byte buffer only to throw
// it away after converting the bytes to a string. We generally want the same
// behavior as BinaryReader, but with a fixed unsigned 16-bit length prefix
// instead of the packed 32-bit one BinaryReader uses for strings by default.
// Unfortunately there's no way to just override the length reading function as
// it's hardcoded to Read7BitEncodedInt in System.IO.BinaryReader.ReadString.
// We could copy ReadString from the .NET Core BinaryReader and just swap the
// hardcoded call with ReadUInt16, but it seems like more pain than it's worth
// considering it depends on internal types and private fields we'd have to
// redefine and allocate too.
//
// Reference: https://github.com/dotnet/runtime/blob/aa5fdab9654d74bc6274c0b5d820272c8e859621/src/libraries/System.Private.CoreLib/src/System/IO/BinaryReader.cs#L276
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
        let mtype = LanguagePrimitives.EnumOfValue (r.ReadByte())
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
            w.Write (uint8 aqid.Type)
            w.Write aqid.Ver
            w.Write aqid.Path
        | Rattach qid ->
            w.Write (uint32 (4+1+2+13))
            w.Write (uint8 MsgType.Rattach)
            w.Write tag
            w.Write (uint8 qid.Type)
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
                w.Write (uint8 q.Type)
                w.Write q.Ver
                w.Write q.Path
        | Ropen (qid, iounit) ->
            w.Write (uint32 (4+1+2+13+4))
            w.Write (uint8 MsgType.Ropen)
            w.Write tag
            w.Write (uint8 qid.Type)
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
        | :? System.IO.IOException as e -> printfn "io error: %s" e.Message

