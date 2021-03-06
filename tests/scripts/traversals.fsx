#load "utils.fsx"
#load "../../src/fraktaal/traversal/learner.fs"
#load "../../src/fraktaal/traversal/sptree.fs"
#load "../../src/fraktaal/traversal/bf.fs"
#load "../../src/fraktaal/traversal/df.fs"
#load "../../src/fraktaal/traversal/ring.fs"

open Utils
open Simplee.DSystems
open Simplee.DSystems.Kernel
open Simplee.DSystems.KFlowBuilder
open Simplee.DSystems.Link.Ops
open Simplee.DSystems.Traversals

let testLearning () =
    let pid0 = ProcessId.ofStr "p0"
    let pid1 = ProcessId.ofStr "p1"
    let pid2 = ProcessId.ofStr "p2"
    let pid3 = ProcessId.ofStr "p3"
    let pid4 = ProcessId.ofStr "p4"
    let pid5 = ProcessId.ofStr "p5"

    kernel {
        do! dbg "<<< Test Learning >>>"
        
        // create the processes
        let! procs = 
            [ pid0; pid1; pid2; pid3; pid4; pid5 ] 
            |> Learner.spawns
            |> KFlow.map Map.ofList

        // create the connections
        do! [
            pid0 <=> pid1
            pid0 <=> pid2
            pid1 <=> pid3
            pid1 <=> pid4
            pid2 <=> pid5
            ] |> addLinks

        do! sleep 100

        // Start the learning
        let! _ = 
            procs 
            |> Map.find pid0
            |> Learner.start (SessionId.ofStr "s0")

        do! sleep 100

        return () }

let testSpanningTree () =
    let pidA = ProcessId.ofStr "a"
    let pidB = ProcessId.ofStr "b"
    let pidC = ProcessId.ofStr "c"
    let pidF = ProcessId.ofStr "f"
    let pidG = ProcessId.ofStr "g"
    let pidI = ProcessId.ofStr "i"
    let pidJ = ProcessId.ofStr "j"
    let pidK = ProcessId.ofStr "k"

    kernel {
        do! dbg "<<< Test Spanning Tree >>>"
        
        // create the processes
        let! procs = 
            [ pidA; pidB; pidC; pidF; pidG; pidI; pidJ; pidK ] 
            |> SpanningTree.spawns
            |> KFlow.map Map.ofList

        // create the connections
        do! [
            pidA <=> pidB
            pidA <=> pidI
            pidA <=> pidJ
            pidB <=> pidC
            pidB <=> pidJ
            pidC <=> pidF
            pidF <=> pidJ
            pidF <=> pidI
            pidF <=> pidK
            pidF <=> pidG
            pidG <=> pidK
            pidI <=> pidJ
            pidI <=> pidK
            ] |> addLinks

        do! sleep 100

        // Start the learning
        let! _ = 
            procs 
            |> Map.find pidA
            |> SpanningTree.start (SessionId.ofStr "s0") 10

        do! sleep 100

        return () }

let testBreadthFirst () =
    let pidA = ProcessId.ofStr "a"
    let pidB = ProcessId.ofStr "b"
    let pidC = ProcessId.ofStr "c"
    let pidD = ProcessId.ofStr "d"

    kernel {
        do! dbg "<<< Test Breadth First >>>"
        
        // create the processes
        let! procs = 
            [ pidA; pidB; pidC; pidD; ] 
            |> BreadthFirst.spawns
            |> KFlow.map Map.ofList

        // create the connections
        do! [
            pidA <=> pidB
            pidB <=> pidC
            pidC <=> pidD
            pidD <=> pidA
            pidA <=> pidC
            pidB <=> pidD
            ] |> addLinks

        do! sleep 100

        // Start the learning
        let! _ = 
            procs 
            |> Map.find pidA
            |> BreadthFirst.start (SessionId.ofStr "s0")

        do! sleep 100

        return () }

let testDepthFirst () =
    let pidA = ProcessId.ofStr "a"
    let pidB = ProcessId.ofStr "b"
    let pidC = ProcessId.ofStr "c"
    let pidD = ProcessId.ofStr "d"

    kernel {
        do! dbg "<<< Test Depth First >>>"
        
        // create the processes
        let! procs = 
            [ pidA; pidB; pidC; pidD ] 
            |> DepthFirst.spawns
            |> KFlow.map Map.ofList

        // create the connections
        do! [
            pidA <=> pidB
            pidB <=> pidC
            pidC <=> pidD
            pidD <=> pidA
            pidA <=> pidC
            pidB <=> pidD
            ] |> addLinks

        do! sleep 100

        // Start the learning
        let! _ = 
            procs 
            |> Map.find pidA
            |> DepthFirst.start (SessionId.ofStr "s0")

        do! sleep 100

        return () }

let testRing () =
    let pid1 = ProcessId.ofStr "p1"
    let pid2 = ProcessId.ofStr "p2"
    let pid3 = ProcessId.ofStr "p3"
    let pid4 = ProcessId.ofStr "p4"
    let pid5 = ProcessId.ofStr "p5"

    kernel {
        do! dbg "<<< Test Ring >>>"
        
        // create the processes
        let! procs = 
            [ pid1; pid2; pid3; pid4; pid5 ] 
            |> Ring.spawns
            |> KFlow.map Map.ofList

        // create the connections
        do! [
            pid1 <=> pid3
            pid1 <=> pid2
            pid2 <=> pid5
            pid2 <=> pid4
            pid2 <=> pid3
            pid3 <=> pid4
            ] |> addLinks

        do! sleep 100

        // Start the learning
        let! _ = 
            procs 
            |> Map.find pid1
            |> Ring.start (SessionId.ofStr "s0")

        do! sleep 100

        let! _ =
            procs
            |> Map.find pid1
            |> Ring.token (SessionId.ofStr "s1") 10

        do! sleep 100

        return () }

[
//testKernel
//testNeighbors
//testLearning
//testSpanningTree
//testBreadthFirst
//testDepthFirst
//testDepthFirst1
testRing
] |> runTests