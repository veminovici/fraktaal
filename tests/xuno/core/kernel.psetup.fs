namespace Testly.DSystems.Kernel

open Xunit
open Xunit.Abstractions

open Simplee.DSystems
open Simplee.DSystems.Kernel

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

type ProcSetupTests(output: ITestOutputHelper) =

    [<Fact>]
    let ``Create process and set its neighbors`` () =

        let pid0 = ProcessId.ofStr "p0"
        let pid1 = ProcessId.ofStr "p1"
        let pid2 = ProcessId.ofStr "p2"

        let getState (pid, proc) = 
            proc
            |> TstKernel.stt
            |> Async.map (fun stt -> pid, stt)

        let ns (p: TstKernelStt) = p.Ns

        let checkP0 res = 
            let p0s = res |> Array.where (fun (pid, _) -> pid = pid0) |> Array.map snd

            p0s
            |> Array.length
            |> (=) 1
            |> Assert.True

            let proc = Array.head p0s

            proc
            |> (fun p -> p.Pid)
            |> (=) pid0
            |> Assert.True

            proc
            |> ns
            |> List.length
            |> (=) 2
            |> Assert.True

            proc
            |> ns
            |> List.where (fun n -> n |> Neighbor.pid |> (=) pid1)
            |> List.head
            |> function
            | Neighbor.CanSendTo _ -> true
            | _                    -> false
            |> Assert.True

            res

        let checkP1 res = 
            let p1s = res |> Array.where (fun (pid, _) -> pid = pid1) |> Array.map snd
            let proc = Array.head p1s

            proc
            |> (fun p -> p.Pid)
            |> (=) pid1
            |> Assert.True

            proc
            |> ns
            |> List.length
            |> (=) 1
            |> Assert.True

            proc
            |> ns
            |> List.where (fun n -> n |> Neighbor.pid |> (=) pid0)
            |> List.head
            |> function
            | Neighbor.CanSendTo _ -> true
            | _                    -> false
            |> Assert.True

            res

        let flow = kernel {
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
        
        flow
        |> runSync
        |> checkP0
        |> checkP1
        |> ignore

    [<Fact>]
    let ``Create process and left and right`` () =

        let pid0 = ProcessId.ofStr "p0"
        let pid1 = ProcessId.ofStr "p1"
        let pid2 = ProcessId.ofStr "p2"

        let getState (pid, proc) = 
            proc
            |> TstKernel.stt
            |> Async.map (fun stt -> pid, stt)

        let ns (p: TstKernelStt) = p.Ns

        let proc p =
            Array.where (fun (pid, _) -> pid = p) 
            >> Array.map snd 
            >> Array.head

        let checkP p (pr, pl) res = 
            let proc = proc p res

            proc
            |> ns
            |> List.length
            |> (=) 2
            |> Assert.True

            proc
            |> ns
            |> List.where (fun n -> n |> Neighbor.pid |> (=) pr)
            |> List.head
            |> function
            | Neighbor.CanSendTo (_, _, d) when d = ToRightDirection -> true
            | _                                                      -> false
            |> Assert.True

            proc
            |> ns
            |> List.where (fun n -> n |> Neighbor.pid |> (=) pl)
            |> List.head
            |> function
            | Neighbor.CanSendTo (_, _, d) when d = ToLeftDirection -> true
            | _                                                     -> false
            |> Assert.True

            res

        let checkP0 = checkP pid0 (pid1, pid2)  
        let checkP1 = checkP pid1 (pid2, pid0) 
        let checkP2 = checkP pid2 (pid0, pid1)

        let flow = kernel {
            // create the processes
            let! procs = 
                [ pid0; pid1; pid2 ] 
                |> TstKernel.spawns
                |> KFlow.map Map.ofList

            // create the connections
            do! [
                (pid0 <=> pid1) ++> ToRightDirection
                (pid1 <=> pid2) ++> ToRightDirection
                (pid2 <=> pid0) ++> ToRightDirection] 
                |> addLinks

            do! sleep 100

            // get states of the processes
            return
                procs 
                |> Map.toList 
                |> List.map getState
                |> Async.Parallel
                |> Async.RunSynchronously }
        
        flow
        |> runSync
        |> checkP0
        |> checkP1
        |> checkP2
        |> ignore
