namespace Simplee.DSystems.Traversals

open Simplee.DSystems

type DepthFirstPub = DFProc of IProcessPub

[<RequireQualifiedAccess>]
module DepthFirst =

    /// The public api messages
    type private ApiMessage =
        | ApiStart
        with
        override this.ToString() = 
            match this with
            | ApiStart -> "START"

    /// The internal messages
    type private IntMessage = 
        | Go
        | Back of bool

    type private SttRunningCtx = {
        Children:    ProcessId list
        ToBeVisited: ProcessId list }
        with
        static member Empty          = { Children = []; ToBeVisited = [] }
        member this.IsCompleted ()   = List.isEmpty this.ToBeVisited
        member this.WithoutVisited n = { Children = this.Children; ToBeVisited = this.ToBeVisited |> List.where (fun p -> p <> n)}

    type private SttDoneCtx = {
        Children: ProcessId list }

    type private State =
        | SttUninitialized
        | SttRootRunning   of SttRunningCtx
        | SttRootDone      of SttDoneCtx
        | SttChildRunning  of ProcessId * SttRunningCtx
        | SttChildDone     of ProcessId * SttDoneCtx
        with
        override this.ToString() = 
            match this with
            | SttUninitialized           -> "uninitialized"
            | SttRootRunning         ctx -> sprintf "root running (%A)"       ctx.ToBeVisited
            | SttRootDone            ctx -> sprintf "root done (%A)"          ctx.Children
            | SttChildRunning (pid, ctx) -> sprintf "child running (%O) (%A)" pid ctx.ToBeVisited
            | SttChildDone    (pid, ctx) -> sprintf "child done (%O) (%A)"    pid ctx.Children

    let private aempty = fun _ -> async { return () }

    [<AutoOpen>]
    module private ActivePatterns =
        let (|Uninitialized|Running|Done|) = function
            | SttUninitialized  as stt -> Uninitialized stt
            | SttRootRunning  _ as stt -> Running       stt
            | SttChildRunning _ as stt -> Running       stt
            | SttRootDone     _ as stt -> Done          stt
            | SttChildDone    _ as stt -> Done          stt

        let (|Root|Child|Unknown|) = function
            | SttUninitialized  as stt -> Unknown stt
            | SttRootRunning  _ as stt -> Root    stt
            | SttRootDone     _ as stt -> Root    stt
            | SttChildRunning _ as stt -> Child   stt
            | SttChildDone    _ as stt -> Child   stt

        let (|RanToCompletion|StillIncomplete|) = function
            | SttRootRunning      ctx  as stt -> 
                if ctx.IsCompleted () 
                then RanToCompletion stt 
                else StillIncomplete stt
            | SttChildRunning (_, ctx) as stt -> 
                if ctx.IsCompleted () 
                then RanToCompletion stt 
                else StillIncomplete stt
            | stt -> 
                failwith (notSupported "RanToCompletion|..." stt)

    [<AutoOpen>]
    module private StateTransitions =
        let moveIntoRootRunning (_: State) = 
            SttRootRunning SttRunningCtx.Empty

        let moveIntoDone = function
            | SttRootRunning        ctx  -> SttRootDone  <| { Children = ctx.Children }
            | SttChildRunning (pid, ctx) -> SttChildDone <| (pid, { Children = ctx.Children })
            | stt                        -> failwith (notSupported "moveIntoDone" stt)

        let moveIntoChildRunning n (_: State) =
            SttChildRunning (ProcessId.ofFromId n, SttRunningCtx.Empty)

        let withToBeVisited ns = function
            | SttRootRunning        ctx  -> SttRootRunning  <| { ctx with ToBeVisited = ns }
            | SttChildRunning (pid, ctx) -> SttChildRunning <| (pid, { ctx with ToBeVisited = ns })
            | stt                        -> failwith (notSupported "withNeighbors" stt)

        let withChildIf p c stt = 
            if p then
                match stt with
                | SttRootRunning        ctx  -> SttRootRunning  <| { ctx with Children = c :: ctx.Children }
                | SttChildRunning (pid, ctx) -> SttChildRunning <| (pid, { ctx with Children = c :: ctx.Children })
                | stt                        -> failwith (notSupported "withChild" stt)
            else
                stt

        let withVisited v = function
            | SttRootRunning        ctx  -> SttRootRunning  <| { ctx with ToBeVisited = ctx.ToBeVisited |> List.where (fun p -> p <> v) }
            | SttChildRunning (pid, ctx) -> SttChildRunning <| (pid, { ctx with ToBeVisited = ctx.ToBeVisited |> List.where (fun p -> p <> v) })
            | stt                        -> failwith (notSupported "withVisited" stt)

        let parent = function
            | SttChildRunning (pid, _) -> pid
            | SttChildDone    (pid, _) -> pid
            | stt                      -> failwith (notSupported "parent" stt)

    [<AutoOpen>]
    module private ToSend = 
        let withSendToNeighbor = function
            | SttRootRunning      ctx  as stt -> 
                match ctx.ToBeVisited with
                | [] -> failwith (notSupported "getNeighborToVisit" stt)
                | x::tail -> stt |> withToBeVisited tail, x
            | SttChildRunning (_, ctx) as stt -> 
                match ctx.ToBeVisited with
                | [] -> failwith (notSupported "getNeighborToVisit" stt)
                | x::tail -> stt |> withToBeVisited tail, x
            | stt -> failwith (notSupported "getNeighborToVisit" stt)

        let withSendToSource env stt =
            let pid = env |> Envelope.fid |> ProcessId.ofFromId
            stt, pid

        let withSendToParent stt =
            stt, parent stt

    [<AutoOpen>]
    module private Envelope =
        open StateTransitions

        let private mkEnvelope pctx sid dest = 
            ()
            |> Envelope.ofRndEid
            |> Envelope.withSid sid
            |> Envelope.withFid (ProcessCtx.fid pctx)
            |> Envelope.withTid dest

        let private goEnvelope pctx sid dest =
            dest
            |> mkEnvelope pctx sid
            |> Envelope.withItem pctx.Serializer Go

        let private backNoEnvelope pctx sid dest =
            dest
            |> mkEnvelope pctx sid
            |> Envelope.withItem pctx.Serializer (Back false)

        let private backYesEnvelope pctx sid dest =
            dest
            |> mkEnvelope pctx sid
            |> Envelope.withItem pctx.Serializer (Back true)

        let withGoEnvelope pctx sid (stt, dest) =

            let env = goEnvelope pctx sid (ToId.ofProcessId dest)
            stt, [env]

        let withBackNoEnvelope pctx sid (stt, dest) =
            let env = backNoEnvelope pctx sid (ToId.ofProcessId dest)
            stt, [env]

        let withBackYesEnvelope pctx sid (stt, dest) =
            let env = backYesEnvelope pctx sid (ToId.ofProcessId dest)
            stt, [env]

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

        let stt, envs, msg, aflow =
            match (msg, stt) with
            | Go, Running stt       -> 
                stt
                |> withSendToSource   env
                |> withBackNoEnvelope pctx sid
                |> withMessage        "go/backno"
                |> withAsyncFlow aempty 
            | Go, Done stt          -> 
                stt
                |> withSendToSource   env
                |> withBackNoEnvelope pctx sid
                |> withMessage        "go/backno"
                |> withAsyncFlow      aempty 
            | Go, Uninitialized stt -> 
                stt
                |> moveIntoChildRunning env.Fid
                |> withToBeVisited      (pctx |> ProcessCtx.procsCanSentToExceptFid env.Fid)
                |> function
                | RanToCompletion stt ->
                    stt
                    |> moveIntoDone
                    |> withSendToSource    env
                    |> withBackYesEnvelope pctx sid
                    |> withMessage         "go/done"
                    |> withAsyncFlow       aempty
                | StillIncomplete stt ->
                    stt
                    |> withSendToNeighbor
                    |> withGoEnvelope pctx sid
                    |> withMessage    "go/go"
                    |> withAsyncFlow  aempty 
            | Back flg, Running stt  ->
                stt
                |> withChildIf flg (ProcessId.ofFromId env.Fid)
                |> function
                | RanToCompletion stt ->
                    stt
                    |> function
                    | Root stt ->
                        stt
                        |> moveIntoDone
                        |> withNoEnvelopes
                        |> withMessage     (sprintf "back%b/done" flg)
                        |> withAsyncFlow   (doDbg "<<< DONE >>>")
                    | Child stt -> 
                        stt
                        |> moveIntoDone
                        |> withSendToParent
                        |> withBackYesEnvelope pctx sid
                        |> withMessage         (sprintf "back%b/done" flg)
                        |> withAsyncFlow       (doDbg "<<< DONE >>>")
                    | _ ->
                        stt
                        |> withNoEnvelopes
                        |> withMessage (sprintf "back%b/err" flg)
                        |> withAsyncFlow aempty
                | StillIncomplete stt ->
                    stt
                    |> withSendToNeighbor
                    |> withGoEnvelope     pctx sid
                    |> withMessage        (sprintf "back%b/go" flg)
                    |> withAsyncFlow      aempty
            | Back flg, stt          ->
                stt, [], sprintf "back%b/err" flg, doErr "We should not be here"

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
            match (api, stt) with
            | (ApiStart, Uninitialized stt) ->
                stt
                |> moveIntoRootRunning
                |> withToBeVisited (pctx |> ProcessCtx.procsCanSentTo)
                |> function
                | RanToCompletion stt ->
                    stt
                    |> moveIntoDone
                    |> withNoEnvelopes
                    |> withMessage     "start/done"
                    |> withAsyncFlow   (doDbg "<<< DONE >>>")
                | StillIncomplete stt ->
                    stt
                    |> withSendToNeighbor
                    |> withGoEnvelope     pctx sid
                    |> withMessage        "start/go" 
                    |> withAsyncFlow      aempty
            | api, stt -> 
                stt
                |> withNoEnvelopes
                |> withMessage     "api/err"
                |> withAsyncFlow   (doErr (sprintf "Unsupported api: (%O)" api))

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
        |> KFlow.map (List.map (fun (pid, p) -> (pid, DFProc p)))
        
    /// Initiates a learning session, with a given
    /// session identifier and starting from a given process.
    let start sid (DFProc p) = 
        Kernel.apiCallWithArgs sid ApiStart p

