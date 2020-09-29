module NineP.Tests.Main

open System

open Xunit
open FsUnit.Xunit

open NineP
open NineP.Server
open NineP.Tests.Data

let makeServer () =
    State.create (fun _ _ -> Ok (Directory defaultTree)) (new System.IO.MemoryStream())

[<Fact>]
let ``Tread on invalid fid`` () =
    let state = makeServer()
    Tread (0u, 0UL, 0u)
    |> NineP.Server.handle state.AttachHandler state.Session 0us
    |> should equal (state.Session, Rerror Enofid)

[<Fact>]
let ``Tread without Topen`` () =
    let state = makeServer()
    let session', rattach =
        Tattach (0u, P2000.NoFid, "", "")
        |> NineP.Server.handle state.AttachHandler state.Session 0us
    let session'', rwalk =
        Twalk (0u, 1u, [|"a"|])
        |> NineP.Server.handle state.AttachHandler session' 0us
    Tread (1u, 0UL, 0u)
    |> NineP.Server.handle state.AttachHandler session'' 0us
    |> should equal (session'', Rerror Enotopen)

[<Fact>]
let ``Tread with offset exactly equal to 1UL<<<63`` () =
    let state = makeServer()
    let session', rattach =
        Tattach (0u, P2000.NoFid, "", "")
        |> NineP.Server.handle state.AttachHandler state.Session 0us
    let session'', rwalk =
        Twalk (0u, 1u, [|"dir1"; "c.fs"|])
        |> NineP.Server.handle state.AttachHandler session' 0us
    let session''', ropen =
        Topen (1u, OpenMode.Read)
        |> NineP.Server.handle state.AttachHandler session'' 0us
    Tread (1u, 1UL<<<63, 0u)
    |> NineP.Server.handle state.AttachHandler session''' 0us
    |> should equal (session''', Rerror Es64off)

[<Fact>]
let ``Tread with offset larger than 1UL<<<63`` () =
    let state = makeServer()
    let session', rattach =
        Tattach (0u, P2000.NoFid, "", "")
        |> NineP.Server.handle state.AttachHandler state.Session 0us
    let session'', rwalk =
        Twalk (0u, 1u, [|"a"|])
        |> NineP.Server.handle state.AttachHandler session' 0us
    let session''', ropen =
        Topen (1u, OpenMode.Read)
        |> NineP.Server.handle state.AttachHandler session'' 0us
    Tread (1u, (1UL<<<63)+1UL, 0u)
    |> NineP.Server.handle state.AttachHandler session''' 0us
    |> should equal (session''', Rerror Es64off)
