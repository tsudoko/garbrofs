module NineP.Tests.Data

open System.Collections.Generic

open NineP
open NineP.Server

// TODO: proper qids with unique paths

type TestingDirectory(name: string, entries: IReadOnlyDictionary<string, Node>) =
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

        member f.Open() =
            Ok (System.IO.File.OpenRead(path) :> _)

type TestingFile(name: string, contents: byte []) =
    interface IFile with
        member f.Stat =
            Stat(mode = 0o444u,
                length = uint64 contents.Length,
                name = name)

        member f.Open() =
            Ok (new System.IO.MemoryStream(contents) :> _)

let defaultTree =
    let toMap (e: Node list) =
        e
        |> List.map (fun x -> x.Stat.Name, x)
        |> Map.ofList
        :> IReadOnlyDictionary<_, _>

    let d4 = TestingDirectory("dir4", Map.empty<string, Node> :> IReadOnlyDictionary<_, _>)
    let fa = TestingFile("a", "awoo"B)
    let fb = TestingFile("b", "AWOO"B)
    let fc = TestingProxyFile("c.fs", "9p.fs")
    let fd = TestingProxyFile("d.fs", "9pserver.fs")
    let d3 = TestingDirectory("dir3", [Directory d4; File fd] |> toMap)
    let d1 = TestingDirectory("dir1", [Directory d3; File fc] |> toMap)
    let d2 = TestingDirectory("dir2", Map.empty<string, Node> :> IReadOnlyDictionary<_, _>)
    TestingDirectory("/", [Directory d1; Directory d2; File fa; File fb] |> toMap)
