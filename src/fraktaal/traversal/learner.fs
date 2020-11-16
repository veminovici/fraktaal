namespace Simplee.DSystems.Traversals

open Simplee.DSystems

type LearnerPub = LearnerProc of IProcessPub

[<RequireQualifiedAccess>]
module Learner =

    /// The public api messages
    type private ApiMessage =
        | ApiStart
        with
        override this.ToString() = 
            match this with
            | ApiStart -> "START"

    /// The internal messages
    type private IntMessage = 
        | Position of ProcessId * ProcessId list

    type SttCtx = {
        Processes: ProcessId list
        Channels:  (ProcessId * ProcessId) list }
        with
        override this.ToString() =
            sprintf "Ps=%A cs=%A" this.Processes this.Channels

    /// The internal state for each learning process.
    type private State = 
        | SttUninitialized
        | SttRunning of SttCtx
        | SttDone    of SttCtx
        with
        override this.ToString() =
            match this with
            | SttUninitialized -> "uninitialized"
            | SttRunning ctx   -> sprintf "RUNG %O" ctx
            | SttDone    ctx   -> sprintf "DONE %O" ctx

    [<AutoOpen>]
    module private ActivePatterns = 
        let (|RanToCompletion|StillIncomplete|) = function
            | SttRunning ctx as stt ->
                let check (xid, yid) =
                    ctx.Processes
                    |> List.fold (fun (fx, fy) p ->
                        match p with
                        | p when p = xid -> (true, fy)
                        | p when p = yid -> (fx, true)
                        | _              -> (fx, fy)) (false, false)

                ctx.Channels
                |> List.fold (fun acc (xid, yid) ->
                    (xid, yid)
                    |> check
                    |> function
                    | true, true -> acc && true
                    | _          -> false) true
                |> function
                | true  -> RanToCompletion stt
                | false -> StillIncomplete stt
            | stt -> failwith (notSupported "RanToCompletion..." stt)

        let (|IsUnknownProcess|_|) p = function
            | SttRunning ctx as stt ->
                ctx.Processes 
                |> List.contains p
                |> function
                | true  -> None
                | false -> Some stt
            | SttDone ctx as stt ->
                ctx.Processes 
                |> List.contains p
                |> function
                | true  -> None
                | false -> Some stt
            | stt -> failwith (notSupported "IsKnownProcess..." stt)

    [<AutoOpen>]
    module private State = 
        let ofPid p = { Processes = [p]; Channels = [] }
        let withChannles cs ctx = { ctx with Channels = cs @ ctx.Channels } 

        let withKnownProcess p = function
            | SttRunning ctx -> SttRunning { ctx with Processes = p :: ctx.Processes }
            | stt            -> failwith (notSupported "withKnownProcess" stt)
        
        let withKnownChannels p ns = function
            | SttRunning ctx -> let cs = ns |> List.map (fun n -> (p, n)) in SttRunning { ctx with Channels = cs @ ctx.Channels }
            | stt            -> failwith (notSupported "withKnownChannels" stt)

    [<AutoOpen>]
    module private StateTransitions =

        let moveIntoRunning (_: State) =
            SttRunning { Processes = []; Channels = []}

    [<AutoOpen>]
    module private SendTo =

        let withSendToNeighbors ns = function
            | SttRunning _ as stt -> stt , ns
            | stt                 -> failwith (notSupported "withSentToNeighbors" stt)

    [<AutoOpen>]
    module private Envelope = 

        let private mkEnvelope pctx sid dest =
            ()
            |> Envelope.ofRndEid
            |> Envelope.withSid sid
            |> Envelope.withFid (ProcessCtx.fid pctx)
            |> Envelope.withTid dest

        let private myPosition pctx = 
            pctx
            |> ProcessCtx.procsCanSentTo
            |> fun ns -> Position (ProcessCtx.pid pctx, ns)

        let withPosEnvelopes pctx sid (stt, ns) =
            let pos = myPosition pctx

            let envs =
                ns
                |> List.map (ToId.ofProcessId >> mkEnvelope pctx sid >> Envelope.withItem pctx.Serializer pos)

            stt, envs

        let withEnvelopes envs' (stt, envs) =
            stt, envs' @ envs

        let withFwdPosEnvelopes pctx sid xid xs (stt, ns) =
            let pos = Position (xid, xs)

            let envs =
                ns
                |> List.map (ToId.ofProcessId >> mkEnvelope pctx sid >> Envelope.withItem pctx.Serializer pos)

            stt, envs

    let private rcv : ReceiveHandle = fun pstt pctx env -> 
        let pid              = pctx.Pid
        let stt : State      = ProcessState.toItem pstt
        let msg : IntMessage = Envelope.msg pctx.Serializer env
        let sid              = env.Sid

        let doTrace   = List.ofItem >> pctx.Trace
        let doRcv     = stt |> sprintf "msg=[%O] stt=[%O]" msg |> Trace.prcv pid env |> doTrace
        let doSnd m e = sprintf "%s stt=[%O]" m >> Trace.psnd pid e >> doTrace
        let doErr m   = sprintf "%s stt=[%O]" m >> Trace.perr pid sid >> doTrace
        let doDbg m   = sprintf "%s stt=[%O]" m >> Trace.pdbg pid sid >> doTrace

        async {
        do! doRcv

        let stt, envs, msg, aflow =
            match msg, stt with
            | Position (xid, ns), (SttUninitialized as stt) ->
                stt
                |> moveIntoRunning
                |> withKnownProcess    pid
                |> withKnownChannels   pid (pctx |> ProcessCtx.procsCanSentTo)
                |> withSendToNeighbors (pctx |> ProcessCtx.procsCanSentTo)
                |> withPosEnvelopes    pctx sid
                |> function
                | IsUnknownProcess xid stt, envs ->
                    stt
                    |> withKnownProcess    xid
                    |> withKnownChannels   xid ns
                    |> withSendToNeighbors (pctx |> ProcessCtx.procsCanSentToExceptFid env.Fid)
                    |> withFwdPosEnvelopes pctx sid xid ns
                    |> withEnvelopes       envs
                | se ->
                    se
                |> function
                | RanToCompletion _, _ as se ->
                    se
                    |> withMessage   "pos/done"
                    |> withAsyncFlow (doDbg "<<< DONE >>>") 
                | StillIncomplete _, _ as se ->
                    se
                    |> withMessage   "pos/pos"
                    |> withAsyncFlow aempty
            | Position (xid, ns), (SttRunning _ as stt)
            | Position (xid, ns), (SttDone    _ as stt) ->
                stt
                |> withNoEnvelopes
                |> function
                | IsUnknownProcess xid stt, envs ->
                    stt
                    |> withKnownProcess  xid
                    |> withKnownChannels xid ns
                    |> withSendToNeighbors (pctx |> ProcessCtx.procsCanSentToExceptFid env.Fid)
                    |> withFwdPosEnvelopes pctx sid xid ns
                    |> withEnvelopes       envs
                | se -> se
                |> function
                | RanToCompletion _, _ as se ->
                    se
                    |> withMessage   "pos/done"
                    |> withAsyncFlow (doDbg "<<< DONE >>>") 
                | StillIncomplete _, _ as se ->
                    se
                    |> withMessage   "pos/pos"
                    |> withAsyncFlow aempty

        do! aflow stt
        do! doSnd msg envs stt
        return ProcessState.ofItem stt, envs }

    let private api : ApiCallHandle = fun pstt pctx sid args -> 
    
        let pid              = pctx.Pid
        let api : ApiMessage = ApiArguments.toItem args
        let stt : State      = ProcessState.toItem pstt
        
        let doTrace   = List.ofItem >> pctx.Trace
        let doApi     = stt |> sprintf "api=[%O] stt=[%O]" api |> Trace.papi pid sid |> doTrace
        let doSnd m e = sprintf "%s stt=[%O]" m >> Trace.psnd pid e >> doTrace
        let doErr m   = sprintf "%s stt=[%O]" m >> Trace.perr pid sid >> doTrace
        let doDbg m   = sprintf "%s stt=[%O]" m >> Trace.pdbg pid sid >> doTrace

        async {
        do! doApi

        let stt, envs, msg, aflow =
            match api with
            | ApiStart ->
                stt
                |> moveIntoRunning
                |> withKnownProcess    pid
                |> withKnownChannels   pid (pctx |> ProcessCtx.procsCanSentTo)
                |> withSendToNeighbors (pctx |> ProcessCtx.procsCanSentTo)
                |> withPosEnvelopes    pctx sid
                |> function
                | RanToCompletion _, _ as se ->
                    se
                    |> withMessage "start/done"
                    |> withAsyncFlow (doDbg "<<< DONE >>>") 
                | StillIncomplete _, _ as se ->
                    se
                    |> withMessage "start/pos"
                    |> withAsyncFlow aempty 

        do! aflow stt
        do! doSnd msg envs stt
        return ProcessState.ofItem stt, envs, ApiResult.nil }

    /// process configuration.
    let private definition =
        ProcessCfg.empty
        >> ProcessCfg.withApi  api
        >> ProcessCfg.withRcv  rcv
        >> ProcessCfg.withZero SttUninitialized

    //
    // Public api
    //

    let spawns ps : KFlow<_> =
        let pcfgs = ps |> List.map definition
        pcfgs 
        |> Kernel.spawns
        |> KFlow.map (List.map (fun (pid, p) -> (pid, LearnerProc p)))

    /// Initiates a learning session, with a given
    /// session identifier and starting from a given process.
    let start sid (LearnerProc p) = 
        Kernel.apiCallWithArgs sid ApiStart p