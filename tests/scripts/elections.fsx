#load "utils.fsx"
#load "../../src/fraktaal/election/leaderUR.fs"

open Utils
open Simplee.DSystems
open Simplee.DSystems.Kernel
open Simplee.DSystems.KFlowBuilder
open Simplee.DSystems.Link.Ops
open Simplee.DSystems.Elections

let testLeaderUR () =
    let pid0 = ProcessId.ofStr "p0"
    let pid1 = ProcessId.ofStr "p1"
    let pid2 = ProcessId.ofStr "p2"
    let pid3 = ProcessId.ofStr "p3"
    let pid4 = ProcessId.ofStr "p4"
    let pid5 = ProcessId.ofStr "p5"

    kernel {
        do! dbg "<<< Test Leader Unidirectional Ring  >>>"
        
        do! sleep 1000

        return () }


[
testLeaderUR
]
|> runTests
