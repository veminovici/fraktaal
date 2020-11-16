namespace Simplee.DSystems.Traversals

open Simplee.DSystems

type BreadthFirstPub = BFProc of IProcessPub

[<RequireQualifiedAccess>]
module BreadthFirst =

    type private ApiMessage =
        | ApiStart
        with
        override this.ToString() = 
            match this with
            | ApiStart -> "START"
    
    type private IntMessage = 
        | Go   of int
        | Back of bool * int

    type private SttRunningCtx = {
        Level:    int
        Children: ProcessId list
        Expected: ProcessId list }
        with 
        static member Empty = { Children = []; Expected = []; Level = 0 }
        member this.IsCompleted () = List.isEmpty this.Expected

    type private SttDoneCtx = {
        Level:    int
        Children: ProcessId list }

    type private State =
        | SttUninitialized
        | SttRootRunning  of SttRunningCtx
        | SttRootDone     of SttDoneCtx
        | SttChildRunning of ProcessId * SttRunningCtx
        | SttChildDone    of ProcessId * SttDoneCtx
        with
        override this.ToString() = 
            match this with
            | SttUninitialized  -> "uninitialized"
            | SttRootRunning  _ -> "root running"
            | SttRootDone     _ -> "root done"
            | SttChildRunning _ -> "child running"
            | SttChildDone    _ -> "child done"

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
            | stt                      -> 
                failwith (notSupported "RanToCompletion|..." stt)

    [<AutoOpen>]
    module private StateTransitions =
        let moveIntoRootRunning (_: State) = 
            SttRootRunning SttRunningCtx.Empty

        let moveIntoDone = function
            | SttRootRunning ctx         -> SttRootDone  <| { Level = ctx.Level; Children = ctx.Children }
            | SttChildRunning (pid, ctx) -> SttChildDone <| (pid, { Level = ctx.Level; Children = ctx.Children })
            | stt                        -> failwith (notSupported "moveIntoDone" stt)

        let moveIntoChildRunning n (_: State) =
            SttChildRunning (ProcessId.ofFromId n, SttRunningCtx.Empty)

        let withExpected ns = function
            | SttRootRunning ctx         -> SttRootRunning  <| { ctx with Expected = ns }
            | SttChildRunning (pid, ctx) -> SttChildRunning <| (pid, { ctx with Expected = ns })
            | stt                        -> failwith (notSupported "withNeighbors" stt)

        let withLevel i = function
            | SttChildRunning (pid, ctx) -> SttChildRunning <| (pid, { ctx with Level = i })
            | stt                        -> failwith (notSupported "withLevel" stt)

        let withChildIf flg c stt = 
            if flg 
            then
                match stt with
                | SttRootRunning ctx         -> SttRootRunning  <| { ctx with Children = c :: ctx.Children }
                | SttChildRunning (pid, ctx) -> SttChildRunning <| (pid, { ctx with Children = c :: ctx.Children })
                | stt                        -> failwith (notSupported "withChild" stt)
            else
                stt

        let withoutExpected n = function
            | SttRootRunning ctx         -> SttRootRunning  <| { ctx with Expected = ctx.Expected |> List.where (fun e -> e <> n) }
            | SttChildRunning (pid, ctx) -> SttChildRunning <| (pid, { ctx with Expected = ctx.Expected |> List.where (fun e -> e <> n) })
            | stt                        -> failwith (notSupported "withoutExpected" stt)

        let level = function
            | SttRootRunning      ctx  -> ctx.Level
            | SttChildRunning (_, ctx) -> ctx.Level
            | stt                      -> failwith (notSupported "level" stt)

        let expected = function
            | SttRootRunning      ctx  -> ctx.Expected
            | SttChildRunning (_, ctx) -> ctx.Expected
            | stt                      -> failwith (notSupported "expected" stt)

        let parent = function
            | SttChildRunning (pid, _) -> pid
            | SttChildDone    (pid, _) -> pid
            | stt                      -> failwith (notSupported "parent" stt)

    [<AutoOpen>]
    module private ToSend = 
        let withSendToNeighbors stt =
            stt, expected stt

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

        let private goEnvelope pctx sid lvl dest =
            dest
            |> mkEnvelope pctx sid
            |> Envelope.withItem pctx.Serializer (Go lvl)

        let private backNoEnvelope pctx sid lvl dest =
            dest
            |> mkEnvelope pctx sid
            |> Envelope.withItem pctx.Serializer (Back (false, lvl))

        let private backYesEnvelope pctx sid lvl dest =
            dest
            |> mkEnvelope pctx sid
            |> Envelope.withItem pctx.Serializer (Back (true, lvl))

        let withGoEnvelopes pctx sid (stt, xs) =

            let lvl = level stt
            let envs = xs |> List.map (ToId.ofProcessId >> goEnvelope pctx sid lvl)

            stt, envs

        let withBackYesEnvelope pctx sid lvl (stt, dest) =
            let env = backYesEnvelope pctx sid lvl (ToId.ofProcessId dest)
            stt, [env]

        let withBackNoEnvelope pctx sid lvl (stt, dest) =
            let env = backNoEnvelope pctx sid lvl (ToId.ofProcessId dest)
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
        
        let stt, envs, msg, aflow =
            match msg, stt with
            | Go lvl, Running stt when level stt > lvl + 1 ->
                stt
                |> moveIntoChildRunning env.Fid
                |> withLevel (lvl + 1)
                |> withExpected (pctx |> ProcessCtx.procsCanSentToExceptFid env.Fid)
                |> function
                | RanToCompletion stt ->
                    stt
                    |> moveIntoDone
                    |> withSendToSource    env
                    |> withBackYesEnvelope pctx sid (lvl + 1)
                    |> withMessage         "go/back/yes"
                    |> withAsyncFlow       (doDbg "<<< DONE >>>")
                | StillIncomplete stt ->
                    stt
                    |> withSendToNeighbors
                    |> withGoEnvelopes    pctx sid
                    |> withMessage        "go/go" 
                    |> withAsyncFlow      aempty
            | Go lvl, Running stt ->
                stt
                |> withSendToSource   env
                |> withBackNoEnvelope pctx sid (lvl + 1)
                |> withMessage        "go/back/no"
                |> withAsyncFlow      aempty
            | Go lvl, Uninitialized stt ->
                stt
                |> moveIntoChildRunning env.Fid
                |> withLevel            (lvl + 1)
                |> withExpected         (pctx |> ProcessCtx.procsCanSentToExceptFid env.Fid)
                |> function
                | RanToCompletion stt ->
                    stt
                    |> moveIntoDone
                    |> withSendToSource    env
                    |> withBackYesEnvelope pctx sid (lvl + 1)
                    |> withMessage         "go/back/yes"
                    |> withAsyncFlow      (doDbg "<<< DONE >>>")
                | StillIncomplete stt ->
                    stt
                    |> withSendToNeighbors
                    |> withGoEnvelopes    pctx sid
                    |> withMessage        "go/go" 
                    |> withAsyncFlow      aempty
            | Back (flg, lvl), Running stt when level stt = lvl - 1 -> 
                stt
                |> withChildIf flg (env.Fid |> ProcessId.ofFromId)
                |> withoutExpected (env.Fid |> ProcessId.ofFromId)
                |> function
                | RanToCompletion stt ->
                    stt
                    |> moveIntoDone
                    |> function
                    | Root stt ->
                        stt
                        |> withNoEnvelopes
                        |> withMessage "back/done"
                        |> withAsyncFlow (doDbg "<<<DONE>>")
                    | Child stt ->
                        stt
                        |> withSendToParent
                        |> withBackYesEnvelope pctx sid (lvl - 1)
                        |> withMessage "back/back/yes"
                        |> withAsyncFlow (doDbg "<<< DONE >>>")
                    | stt ->
                        stt
                        |> withNoEnvelopes
                        |> withMessage "back/err"
                        |> withAsyncFlow (doErr "Unexpected root/child")
                | StillIncomplete stt ->
                    stt
                    |> withNoEnvelopes
                    |> withMessage        "back/running" 
                    |> withAsyncFlow      aempty
            | msg, stt ->
                stt
                |> withNoEnvelopes
                |> withMessage     "msg/err"
                |> withAsyncFlow   (doErr (sprintf "Unsupported msg (%O)" msg))

        async {
        do! doRcv
        do! aflow stt
        do! doSnd msg envs stt
        return ProcessState.ofItem stt, envs }
        
    let private api : ApiCallHandle = fun pstt pctx sid args -> 
        let pid              = pctx.Pid
        let stt : State      = ProcessState.toItem pstt
        let api : ApiMessage = ApiArguments.toItem args

        let doTrace   = List.ofItem >> pctx.Trace
        let doApi     = stt |> sprintf "api=[%O] stt=[%O]" api |> Trace.papi pid sid |> doTrace
        let doSnd m e = sprintf "%s stt=[%O]" m >> Trace.psnd pid e   >> doTrace
        let doErr m   = sprintf "%s stt=[%O]" m >> Trace.perr pid sid >> doTrace
        let doDbg m   = sprintf "%s stt=[%O]" m >> Trace.pdbg pid sid >> doTrace

        let stt, envs, msg, aflow =
            match api, stt with
            | ApiStart, Uninitialized stt ->
                stt
                |> moveIntoRootRunning
                |> withExpected (pctx |> ProcessCtx.procsCanSentTo)
                |> function
                | RanToCompletion stt ->
                    stt
                    |> moveIntoDone
                    |> withNoEnvelopes
                    |> withMessage     "start/done"
                    |> withAsyncFlow   (doDbg "<<< DONE >>>")
                | StillIncomplete stt ->
                    stt
                    |> withSendToNeighbors
                    |> withGoEnvelopes    pctx sid
                    |> withMessage        "start/go" 
                    |> withAsyncFlow      aempty
            | api, stt ->
                stt
                |> withNoEnvelopes
                |> withMessage "api/err"
                |> withAsyncFlow (doErr (sprintf "Unsupported api (%O)" api))

        async {
        do! doApi
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
        |> KFlow.map (List.map (fun (pid, p) -> (pid, BFProc p)))

    /// Initiates a learning session, with a given
    /// session identifier and starting from a given process.
    let start sid (BFProc p) = 
        Kernel.apiCallWithArgs sid ApiStart p