#r "nuget: FsPickler"

#load "../../src/fraktaal/core/kernel.fs"

open Simplee.DSystems
open Simplee.DSystems.Kernel
open Simplee.DSystems.KFlowBuilder
open Simplee.DSystems.Link.Ops

let runTests pred ts = 

    let flow = 
        ts
        |> List.map (fun t -> t ())
        |> KFlow.sequenceA

    kernel {
        let! _ = flow
        return! logs
    }
    |> runSync
    |> List.where pred
    |> List.toStringWithTtl "Combined Logs:"
    |> printfn "%s"

