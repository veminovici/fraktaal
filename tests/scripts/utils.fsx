#r "nuget: FsPickler"

#load "../../src/fraktaal/core/kernel.fs"

open Simplee.DSystems
open Simplee.DSystems.Kernel
open Simplee.DSystems.KFlowBuilder
open Simplee.DSystems.Link.Ops

let runTests ts = 

    let flow = 
        ts
        |> List.map (fun t -> t ())
        |> KFlow.sequenceA

    kernel {
        let! _ = flow
        return! logs
    }
    |> runSync
    //|> List.where LogEntry.isProcLog
    |> List.where LogEntry.isPDbgLog
    |> List.toStringWithTtl "Combined Logs:"
    |> printfn "%s"

