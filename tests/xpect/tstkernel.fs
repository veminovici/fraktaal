namespace Testlee.DSystems

open Expecto

open Simplee.DSystems
open Simplee.DSystems.Kernel
open Simplee.DSystems.KFlowBuilder
open Simplee.DSystems.Link.Ops

type TstKernelPub = TstKernelPub of IProcessPub

[<RequireQualifiedAccess>]
module TstKernel =

    let private cfg : ConfigHandle = fun pstt pctx ns -> async {
        return pstt, [] }

    let private rcv : ReceiveHandle = fun pstt pctx env -> async {
        return pstt, [] }

    let private api : ApiCallHandle = fun pstt pctx sid args -> async {
        let pid              = pctx.Pid
        //let api : ApiMessage = ApiArguments.toItem args
        //let stt : State      = ProcessState.toItem pstt

        return pstt, [], ApiResult.nil }

    /// process configuration.
    let private definition =
        ProcessCfg.empty
        >> ProcessCfg.withApi  api
        >> ProcessCfg.withRcv  rcv
        >> ProcessCfg.withCfg  cfg
        >> ProcessCfg.withZero None

    let spawns ps : KFlow<_> =
        let pcfgs = ps |> List.map definition
        pcfgs 
        |> Kernel.spawns
        |> KFlow.map (List.map (fun (pid, p) -> (pid, TstKernelPub p)))

    let pid (TstKernelPub proc) = proc.Id
    let stt (TstKernelPub proc) = proc.ProcState |> Async.map ProcessState.toItem

module Kernel = 
    [<Tests>]
    let tests = 
        testList "graph" [
            testCase "Process Id is correct" <| fun _ ->
                let pid0 = ProcessId.ofStr "p0"
                let pid1 = ProcessId.ofStr "p1"
                let pid2 = ProcessId.ofStr "p2"

                let flow = 
                    kernel {
                        // create the processes
                        let! procs = 
                            [ pid0; pid1; pid2 ] 
                            |> TstKernel.spawns
                            |> KFlow.map Map.ofList

                        // create the connections
                        do! [ pid0 <=> pid1; pid0 <=> pid2] |> addLinks

                        do! sleep 100

                        // get the state
                        let p0 = procs |> Map.find pid0

                        let res = async {
                            let! pid = TstKernel.pid p0
                            return pid = pid0 } |> Async.RunSynchronously
                        
                        return res }

                let res = flow |> runSync

                //let subject = true
                Expect.isTrue res "The process id is correct"

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
