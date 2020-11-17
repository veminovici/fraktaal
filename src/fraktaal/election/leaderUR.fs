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
        | SttUninitialized
        | SttRunning
        with
        override this.ToString() =
            match this with
            | SttUninitialized -> "uninitialized"
            | SttRunning       -> "running"

    [<RequireQualifiedAccess>]
    module private State =
        let empty = SttUninitialized

    let private rcv : ReceiveHandle = fun pstt pctx env -> 
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

        let stt, envs, msg, aflow = stt, [], "", aempty

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

        let stt, envs, msg, aflow = stt, [], "", aempty

        do! aflow stt
        do! doSnd msg envs stt
        return ProcessState.ofItem stt, envs, ApiResult.nil }

    /// process configuration.
    let private definition =
        ProcessCfg.empty
        >> ProcessCfg.withApi  api
        >> ProcessCfg.withRcv  rcv
        >> ProcessCfg.withZero State.empty

    //
    // Public api
    //

    let spawns ps : KFlow<_> =
        let pcfgs = ps |> List.map definition
        pcfgs 
        |> Kernel.spawns
        |> KFlow.map (List.map (fun (pid, p) -> (pid, LeaderURProc p)))

    /// Initiates a learning session, with a given
    /// session identifier and starting from a given process.
    let start sid dt (LeaderURProc p) = 
        Kernel.apiCallWithArgs sid ApiStart p