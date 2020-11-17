namespace Simplee.DSystems.Elections

open Simplee.DSystems

type LeaderURProc = LeaderURProc of IProcessPub

[<RequireQualifiedAccess>]
module LeaderUR =

    /// The public api messages
    type private ApiMessage =
        | ApiStart
        with
        override this.ToString() = 
            match this with
            | ApiStart -> "START"
            
    type private IntMessage = 
        | Election of ProcessId
        | Elected  of ProcessId

    type private State =
        | SttElection
        | SttLead
        | SttNoLead   of ProcessId
        with
        override this.ToString() =
            match this with
            | SttElection   -> "election"
            | SttLead       -> "lead"
            | SttNoLead xid -> sprintf "done ldr=%O" xid

    [<RequireQualifiedAccess>]
    module private State =
        let empty = SttElection

    [<AutoOpen>]
    module private StateTransitions =


        let moveToElected (_: State) =
            SttLead

        let moveToDone xid (_: State) =
            SttNoLead xid

    [<AutoOpen>]
    module private Envelope =

        let private mkEnvelope pctx sid dest = 
            ()
            |> Envelope.ofRndEid
            |> Envelope.withSid sid
            |> Envelope.withFid (ProcessCtx.fid pctx)
            |> Envelope.withTid dest

        let withElectionEnvelope pctx sid xid stt =
            let envs = 
                pctx
                |> ProcessCtx.downstream
                |> ToId.ofProcessId
                |> mkEnvelope pctx sid
                |> Envelope.withItem pctx.Serializer (Election xid)
                |> List.ofItem

            stt, envs

        let withElectedEnvelope pctx sid xid stt =
            let envs = 
                pctx
                |> ProcessCtx.downstream
                |> ToId.ofProcessId
                |> mkEnvelope pctx sid
                |> Envelope.withItem pctx.Serializer (Elected xid)
                |> List.ofItem

            stt, envs

    let private rcv (comp: Compare<ProcessId>) : ReceiveHandle = fun pstt pctx env -> 
        let pid              = pctx.Pid
        let stt : State      = ProcessState.toItem pstt
        let msg : IntMessage = Envelope.msg pctx.Serializer env
        let sid              = env.Sid

        let doTrace   = List.ofItem >> pctx.Trace
        let doRcv     = msg |> sprintf "stt=[%O] msg=[%O]" stt |> Trace.prcv pid env |> doTrace
        let doSnd m e = sprintf "%s stt=[%O]" m >> Trace.psnd pid e   >> doTrace
        let doErr m   = sprintf "%s stt=[%O]" m >> Trace.perr pid sid >> doTrace
        let doDbg m   = sprintf "%s stt=[%O]" m >> Trace.pdbg pid sid >> doTrace

        async {
        do! doRcv

        let stt, envs, msg, aflow = 
            match msg, stt with
            | Election xid, (SttElection as stt) when comp xid pid = AGreaterThanB ->
                stt
                |> withElectionEnvelope pctx sid xid
                |> withMessage          "election/fwd"
                |> withAsyncFlow        aempty
            | Election xid, (SttElection as stt) when comp xid pid = ALessThanB ->
                stt
                |> withElectionEnvelope pctx sid (ProcessCtx.pid pctx)
                |> withMessage          "election/me"
                |> withAsyncFlow        aempty
            | Election xid, stt when comp xid pid = AEqualToB ->
                stt
                |> moveToElected
                |> withElectedEnvelope pctx sid pid
                |> withMessage         "election/elected"
                |> withAsyncFlow       (doDbg "<<< ELECTED >>> ")
            | Elected xid, (SttElection as stt) ->
                stt
                |> moveToDone xid
                |> withElectedEnvelope pctx sid xid
                |> withMessage         "elected/fwd"
                |> withAsyncFlow       (doDbg "<<< DONE >>>")
            | _, stt ->
                stt
                |> withNoEnvelopes
                |> withMessage     "test"
                |> withAsyncFlow   aempty

        do! aflow stt
        do! doSnd msg envs stt
        return ProcessState.ofItem stt, envs }

    let private api : ApiCallHandle = fun pstt pctx sid args -> 
        let pid              = pctx.Pid
        let api : ApiMessage = ApiArguments.toItem args
        let stt : State      = ProcessState.toItem pstt

        let doTrace   = List.ofItem >> pctx.Trace
        let doApi     = stt |> sprintf "api=[%O] stt=[%O]" api |> Trace.papi pid sid |> doTrace
        let doSnd m e = sprintf "%s stt=[%O]" m >> Trace.psnd pid e   >> doTrace
        let doErr m   = sprintf "%s stt=[%O]" m >> Trace.perr pid sid >> doTrace
        let doDbg m   = sprintf "%s stt=[%O]" m >> Trace.pdbg pid sid >> doTrace

        async {
        do! doApi

        let stt, envs, msg, aflow = 
            match api, stt with
            | ApiStart, (SttElection as stt) -> 
                stt
                |> withElectionEnvelope pctx sid pid
                |> withMessage          "start/election"
                |> withAsyncFlow        aempty
            | ApiStart, stt ->
                stt, [], "api/none", aempty

        do! aflow stt
        do! doSnd msg envs stt
        return ProcessState.ofItem stt, envs, ApiResult.nil }

    /// process configuration.
    let private definition comp =
        ProcessCfg.empty
        >> ProcessCfg.withApi  api
        >> ProcessCfg.withRcv  (rcv comp)
        >> ProcessCfg.withZero State.empty

    //
    // Public api
    //

    let spawns comp ps : KFlow<_> =
        let pcfgs = ps |> List.map (definition comp)
        pcfgs 
        |> Kernel.spawns
        |> KFlow.map (List.map (fun (pid, p) -> (pid, LeaderURProc p)))

    /// Initiates a learning session, with a given
    /// session identifier and starting from a given process.
    let start sid (LeaderURProc p) = 
        Kernel.apiCallWithArgs sid ApiStart p