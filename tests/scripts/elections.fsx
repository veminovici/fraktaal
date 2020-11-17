#load "utils.fsx"
#load "../../src/fraktaal/election/leaderUR.fs"

open Utils
open Simplee.DSystems
open Simplee.DSystems.Kernel
open Simplee.DSystems.KFlowBuilder
open Simplee.DSystems.Link.Ops
open Simplee.DSystems.Elections

let testLeaderUR () =
    let pid1 = ProcessId.ofStr "p1"
    let pid2 = ProcessId.ofStr "p2"
    let pid3 = ProcessId.ofStr "p3"
    let pid4 = ProcessId.ofStr "p4"
    let pid5 = ProcessId.ofStr "p5"

    let comp (xid: ProcessId) (yid: ProcessId) =
        if (xid < yid)   then ALessThanB
        elif (xid > yid) then AGreaterThanB
        else                  AEqualToB

    kernel {
        do! dbg "<<< Test Leader Unidirectional Ring >>>"
        
        // create the processes
        let! procs = 
            [ pid1; pid2; pid3; pid4; pid5 ] 
            |> LeaderUR.spawns comp
            |> KFlow.map Map.ofList

        // create the connections
        do! [
            pid1 =>> pid2
            pid2 =>> pid3
            pid3 =>> pid4
            pid4 =>> pid5
            pid5 =>> pid1
            ] |> addLinks

        do! sleep 100

        // Start the learning
        let! _ = 
            procs 
            |> Map.find pid1
            |> LeaderUR.start (SessionId.ofStr "s0")

        do! sleep 100

        return () }

[
testLeaderUR
]
|> runTests LogEntry.isPDbgLog

//(fun _ -> true)
//LogEntry.isProcLog
//LogEntry.isPDbgLog

