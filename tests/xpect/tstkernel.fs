namespace Testlee.DSystems

open Expecto

open Simplee.DSystems
open Simplee.DSystems.Kernel
open Simplee.DSystems.KFlowBuilder
open Simplee.DSystems.Link.Ops

type TstKernelPub = TstKernelPub of IProcessPub

type TstKernelStt = {
    Pid: ProcessId
    Ns:  Neighbor list }

[<RequireQualifiedAccess>]
module TstKernel =

    let private empty = {
        Pid = ProcessId.empty
        Ns = [] }

    let withPid pid (stt: TstKernelStt) = { stt with Pid = pid }
    let withNeighbors ns (stt: TstKernelStt) = { stt with Ns = ns @ stt.Ns }

    let private cfg : ConfigHandle = fun pstt pctx ns -> async {
        let pid                = pctx.Pid
        let stt : TstKernelStt = ProcessState.toItem pstt

        let stt = stt |> withNeighbors ns |> withPid pid

        return ProcessState.ofItem stt, [] }

    let private rcv : ReceiveHandle = fun pstt pctx env -> async {
        return pstt, [] }

    let private api : ApiCallHandle = fun pstt pctx sid args -> async {
        let pid              = pctx.Pid
        //let api : ApiMessage = ApiArguments.toItem args
        let stt : TstKernelStt = ProcessState.toItem pstt

        return ProcessState.ofItem stt, [], ApiResult.nil }

    /// process configuration.
    let private definition =
        ProcessCfg.empty
        >> ProcessCfg.withApi  api
        >> ProcessCfg.withRcv  rcv
        >> ProcessCfg.withCfg  cfg
        >> ProcessCfg.withZero empty

    let spawns ps : KFlow<_> =
        let pcfgs = ps |> List.map definition
        pcfgs 
        |> Kernel.spawns
        |> KFlow.map (List.map (fun (pid, p) -> (pid, TstKernelPub p)))

    let pid (TstKernelPub proc) = proc.Id
    let stt (TstKernelPub proc) : Async<TstKernelStt> = proc.ProcState |> Async.map ProcessState.toItem

module Kernel = 
    let pid0 = ProcessId.ofStr "p0"
    let pid1 = ProcessId.ofStr "p1"
    let pid2 = ProcessId.ofStr "p2"

    [<Tests>]
    let tests = 
        testList "kernel" [
            testCase "Unifirectional link" <| fun _ ->
                let lnk = pid0 =>> pid1

                match lnk with
                | OneWay _ ->
                    Expect.isTrue true "The link must be unidirectional"
                | _        ->
                    Expect.isTrue false "The link must be unidirectional"
                
                ()

            testCase "Process is correct" <| fun _ ->

                let getState (pid, proc) = 
                    proc
                    |> TstKernel.stt
                    |> Async.map (fun stt -> pid, stt)

                let checkP0 res = 
                    let p0s = res |> Array.where (fun (pid, proc) -> pid = pid0) |> Array.map snd
                    Expect.isTrue (Array.length p0s = 1) "There is only one pid0"

                    let proc0 = Array.head p0s
                    Expect.isTrue (proc0.Pid = pid0) "The identifier for pid0 must be right"

                    let ns = proc0.Ns
                    Expect.isTrue (List.length ns = 2) "The pid0 has two neighbors"

                    res

                kernel {
                    // create the processes
                    let! procs = 
                        [ pid0; pid1; pid2 ] 
                        |> TstKernel.spawns
                        |> KFlow.map Map.ofList

                    // create the connections
                    do! [ pid0 <=> pid1; pid0 <=> pid2] |> addLinks

                    do! sleep 100

                    // get states of the processes
                    return
                        procs 
                        |> Map.toList 
                        |> List.map getState
                        |> Async.Parallel
                        |> Async.RunSynchronously }
                |> runSync
                |> checkP0
                |> ignore

      (*testCase "when true is not (should fail)" <| fun _ ->
        let subject = false
        Expect.isTrue subject "I should fail because the subject is false"

      testCase "I'm skipped (should skip)" <| fun _ ->
        Tests.skiptest "Yup, waiting for a sunny day..."

      testCase "I'm always fail (should fail)" <| fun _ ->
        Tests.failtest "This was expected..."

      testCase "contains things" <| fun _ ->
        Expect.containsAll [| 2; 3; 4 |] [| 2; 4 |]
                           "This is the case; {2,3,4} contains {2,4}"

      testCase "contains things (should fail)" <| fun _ ->
        Expect.containsAll [| 2; 3; 4 |] [| 2; 4; 1 |]
                           "Expecting we have one (1) in there"

      testCase "Sometimes I want to ༼ノಠل͟ಠ༽ノ ︵ ┻━┻" <| fun _ ->
        Expect.equal "abcdëf" "abcdef" "These should equal"

      test "I am (should fail)" {
        "╰〳 ಠ 益 ಠೃ 〵╯" |> Expect.equal true false
      }*)
    ]
