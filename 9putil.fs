module NineP.Util

open System.Collections.Generic

open NineP.Server

type private RecMap<'k, 'v when 'k: comparison> =
    | Terminator of 'v
    | Map of Map<'k, RecMap<'k, 'v>>
let rec private mapMerge<'file> (f: 'file, path: string list) (map: Map<string, RecMap<string, 'file>>) =
    match path with
    | last :: [] -> map |> Map.add last (Terminator f)
    | top :: rest ->
        match map |> Map.tryFind top with
        | Some (RecMap.Map m) -> m
        | _ -> Map.empty
        |> mapMerge (f, rest)
        |> (fun x -> Map.add top (RecMap.Map x) map)
    | [] -> failwith "no terminator"

let rec private treeOfRecMap makeFile makeDirectory ((entries: Map<string, Node>), curQidPath) k v =
    match v with
    | Terminator (index, t) ->
        entries |> Map.add k (makeFile k t (uint64 index)), curQidPath
    | RecMap.Map m ->
        let d, curQidPath' =
            m
            |> Map.fold (treeOfRecMap makeFile makeDirectory) (Map.empty, curQidPath)
            |> fun (x, p) -> x, (p+1UL)
            |> fun (x, p) -> makeDirectory k x p, p
        entries |> Map.add k d, curQidPath'

let deflatten<'a> (makeFile: string -> 'a -> uint64 -> Node)
                  (makeDirectory: string -> Map<string, Node> -> uint64 -> Node)
                  (getPath: 'a -> string)
                  (pathSeparators: char [])
                  (entries: seq<'a>) =
    entries
    |> Seq.indexed
    |> Seq.map (fun (index, f) -> (index, f), (getPath f).Split(pathSeparators) |> List.ofArray)
    |> Seq.fold (fun map (f, path) -> map |> mapMerge (f, path)) Map.empty
    |> Map.fold (treeOfRecMap makeFile makeDirectory) (Map.empty, 0UL)
    |> fun (l, _) -> l :> IReadOnlyDictionary<_, _>
