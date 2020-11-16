namespace Simplee.DSystems.Traversals

open Simplee.DSystems

type RingPub = RingProc of IProcessPub

[<RequireQualifiedAccess>]
module Ring =

    type private ApiMessage =
        | ApiStart
        | ApiToken of int
        with
        override this.ToString() = 
            match this with
            | ApiStart   -> "START"
            | ApiToken t -> sprintf "TOKEN (%d)" t

    type private IntMessage = 
        | Go    of ProcessId list * ProcessId
        | Back  of ProcessId list * ProcessId
        | Token of int * ProcessId
        with
        override this.ToString() =
            match this with
            | Go   (vs, last) -> sprintf "GO %A last=%O"   vs last
            | Back (vs, last) -> sprintf "BACK %A last=%O" vs last
            | Token (t, dest) -> sprintf "TOKEN %d (%O)"   t  dest

    type private SttRunningCtx = {
        First:       ProcessId
        Successor:   ProcessId
        Routing:     Map<FromId, ToId>
        ToBeVisited: ProcessId list }
        with
        static member Empty          = { Routing = Map.empty; ToBeVisited = []; First = ProcessId.empty; Successor = ProcessId.empty }
        member this.IsCompleted vs   = this.ToBeVisited |> List.sublistOf vs
        member this.WithoutVisited n = { Routing = this.Routing; First = this.First; Successor = this.Successor; ToBeVisited = this.ToBeVisited |> List.where (fun p -> p <> n)}

    type private SttDoneCtx = {
        Successor: ProcessId
        Routing:   Map<FromId, ToId> }

    type private State =
        | SttUninitialized
        | SttRootRunning   of SttRunningCtx
        | SttRootDone      of SttDoneCtx
        | SttChildRunning  of ProcessId * SttRunningCtx
        | SttChildDone     of ProcessId * SttDoneCtx
        | SttTokenized     of int       * SttDoneCtx
        with
        override this.ToString() = 
            match this with
            | SttUninitialized           -> "uninitialized"
            | SttRootRunning         ctx -> sprintf "root  running (%A) fst=(%O) succ=(%O) routing=%O" ctx.ToBeVisited ctx.First ctx.Successor ctx.Routing
            | SttRootDone            ctx -> sprintf "root  done successor=%O routing=%A" ctx.Successor ctx.Routing
            | SttChildRunning (pid, ctx) -> sprintf "child running (%A) fst=(%O) succ=(%O) routing=%O" ctx.ToBeVisited ctx.First ctx.Successor ctx.Routing
            | SttChildDone    (pid, ctx) -> sprintf "child done successor=%O routing=%A" ctx.Successor ctx.Routing
            | SttTokenized    (tkn, ctx) -> sprintf "tokenized %d" tkn

    let private aempty = fun _ -> async { return () }

    [<AutoOpen>]
    module private ActivePatterns =
        let (|Uninitialized|Running|Done|Tokenized|) = function
            | SttUninitialized  as stt -> Uninitialized stt
            | SttRootRunning  _ as stt -> Running       stt
            | SttChildRunning _ as stt -> Running       stt
            | SttRootDone     _ as stt -> Done          stt
            | SttChildDone    _ as stt -> Done          stt
            | SttTokenized    _ as stt -> Tokenized     stt

        let (|Root|Child|Unknown|) = function
            | SttUninitialized  as stt -> Unknown stt
            | SttRootRunning  _ as stt -> Root    stt
            | SttRootDone     _ as stt -> Root    stt
            | SttChildRunning _ as stt -> Child   stt
            | SttChildDone    _ as stt -> Child   stt
            | SttTokenized    _ as stt -> Unknown stt

        let (|RanToCompletion|StillIncomplete|) (stt, vs) =
            match stt with
            | SttRootRunning      ctx  as stt -> 
                if ctx.IsCompleted vs 
                then RanToCompletion stt 
                else StillIncomplete stt
            | SttChildRunning (_, ctx) as stt -> 
                if ctx.IsCompleted vs 
                then RanToCompletion stt 
                else StillIncomplete stt
            | stt                      -> 
                printfn "Should not be here!"
                failwith (notSupported "RanToCompletion|..." stt)

    [<AutoOpen>]
    module private StateTransitions =
        let moveIntoRootRunning (_: State) = 
            SttRootRunning SttRunningCtx.Empty

        let moveIntoDone = function
            | SttRootRunning        ctx  -> SttRootDone  <| { Successor = ctx.Successor; Routing = ctx.Routing }
            | SttChildRunning (pid, ctx) -> SttChildDone <| (pid, { Successor = ctx.Successor; Routing = ctx.Routing })
            | stt                        -> failwith (notSupported "moveIntoDone" stt)

        let moveIntoTokenized tkn = function
            | SttRootDone      ctx  -> SttTokenized (tkn, ctx)
            | SttChildDone (_, ctx) -> SttTokenized (tkn, ctx)
            | stt                   -> failwith (notSupported "moveIntoTokenized" stt)

        let moveIntoChildRunning n (_: State) =
            SttChildRunning (ProcessId.ofFromId n, SttRunningCtx.Empty)

        let withToBeVisited ns = function
            | SttRootRunning        ctx  -> SttRootRunning  <| { ctx with ToBeVisited = ns }
            | SttChildRunning (pid, ctx) -> SttChildRunning <| (pid, { ctx with ToBeVisited = ns })
            | stt                        -> failwith (notSupported "withToBeVisited" stt)

        let private withFirst n stt =
            match stt with
            | SttRootRunning        ctx  -> SttRootRunning  <| { ctx with First = n }
            | SttChildRunning (pid, ctx) -> SttChildRunning <| (pid, { ctx with First = n })
            | stt                        -> failwith (notSupported "withFirst" stt)

        let withFirstProcess (stt, n) =
            stt |> withFirst n, n

        let withSuccessor n = function
            | SttRootRunning        ctx  -> SttRootRunning  <| { ctx with Successor = n }
            | SttChildRunning (pid, ctx) -> SttChildRunning <| (pid, { ctx with Successor = n })
            | stt                        -> failwith (notSupported "withVisited" stt)

        let withRouting tid (stt, xid) = 
            let fid = FromId.ofProcessId xid
            let stt = 
                match stt with
                | SttRootRunning        ctx  -> SttRootRunning  <| { ctx with Routing = ctx.Routing |> Map.add fid tid }
                | SttChildRunning (pid, ctx) -> SttChildRunning <| (pid, { ctx with Routing = ctx.Routing |> Map.add fid tid })
                | SttRootDone           ctx  -> SttRootDone     <| { ctx with Routing = ctx.Routing |> Map.add fid tid }
                | SttChildDone    (pid, ctx) -> SttChildDone    <| (pid, { ctx with Routing = ctx.Routing |> Map.add fid tid })
                | stt                        -> failwith (notSupported "withRouting" stt)
            stt, xid

        let withRoutingFst tid = function 
            | SttRootRunning        ctx  -> SttRootRunning  <| { ctx with Routing = ctx.Routing |> Map.add (FromId.ofProcessId ctx.First) tid }
            | SttChildRunning (pid, ctx) -> SttChildRunning <| (pid, { ctx with Routing = ctx.Routing |> Map.add (FromId.ofProcessId ctx.First) tid })
            | stt                        -> failwith (notSupported "withRouting" stt)

        let withVisited v = function
            | SttRootRunning        ctx  -> SttRootRunning  <| { ctx with ToBeVisited = ctx.ToBeVisited |> List.where (fun p -> p <> v) }
            | SttChildRunning (pid, ctx) -> SttChildRunning <| (pid, { ctx with ToBeVisited = ctx.ToBeVisited |> List.where (fun p -> p <> v) })
            | stt                        -> failwith (notSupported "withVisited" stt)

        let parent = function
            | SttChildRunning (pid, _) -> pid
            | SttChildDone    (pid, _) -> pid
            | stt                      -> failwith (notSupported "parent" stt)

        let successor = function
            | SttRootDone      ctx  -> ctx.Successor
            | SttChildDone (_, ctx) -> ctx.Successor
            | stt                   -> failwith (notSupported "successor" stt)

    [<AutoOpen>]
    module private ToSend = 

        let rec firstExcept vs xs =
            match xs with
            | []                              -> failwith "We should not be here"
            | x::tail when List.contains x vs -> firstExcept vs tail
            | x::_                            -> x

        let withSendToNeighborExcept vs = function
            | SttRootRunning      ctx  as stt -> 
                let x = ctx.ToBeVisited |> firstExcept vs
                let xs = ctx.ToBeVisited |> List.where (fun v -> v <> x)
                stt |> withToBeVisited xs, x
            | SttChildRunning (_, ctx) as stt -> 
                let x = ctx.ToBeVisited |> firstExcept vs
                let xs = ctx.ToBeVisited |> List.where (fun v -> v <> x)
                stt |> withToBeVisited xs, x
            | stt -> 
                failwith (notSupported "withSendToNeighborExcept" stt)

        let withSendToNeighbor = function
            | SttRootRunning      ctx  as stt -> 
                match ctx.ToBeVisited with
                | [] -> failwith (notSupported "withSendToNeighbor" stt)
                | x::tail -> 
                    stt |> withToBeVisited tail, x
            | SttChildRunning (_, ctx) as stt -> 
                match ctx.ToBeVisited with
                | [] -> failwith (notSupported "withSendToNeighbor" stt)
                | x::tail -> 
                    stt |> withToBeVisited tail, x
            | stt -> 
                failwith (notSupported "withSendToNeighbor" stt)

        let withSendToSource env stt =
            let pid = env |> Envelope.fid |> ProcessId.ofFromId
            stt, pid

        let withSendToParent stt =
            stt, parent stt
         
        let withSendToRoute fid = function
            | SttTokenized (_, ctx) as stt -> stt, ctx.Routing |> Map.find fid |> ToId.toProcessId
            | stt                          -> failwith (notSupported "withSendToRoute" stt)

        let withSendToRouteAny = function
            | SttTokenized (_, ctx) as stt -> stt, ctx.Routing |> Map.toList |> List.head |> snd |> ToId.toProcessId
            | stt                          -> failwith (notSupported "withSendToRoute" stt)

    [<AutoOpen>]
    module private Envelope =

        let private mkEnvelope pctx sid dest = 
            ()
            |> Envelope.ofRndEid
            |> Envelope.withSid sid
            |> Envelope.withFid (ProcessCtx.fid pctx)
            |> Envelope.withTid dest

        let private goEnvelope pctx sid vs dest =
            dest
            |> mkEnvelope pctx sid
            |> Envelope.withItem pctx.Serializer (Go vs)

        let withGoEnvelope pctx sid vs last (stt, dest) =
            let envs = 
                dest
                |> ToId.ofProcessId
                |> mkEnvelope pctx sid
                |> Envelope.withItem pctx.Serializer (Go (vs, last))
                |> List.ofItem

            stt, envs

        let withBackEnvelope pctx sid vs last (stt, dest) =
            let envs = 
                dest
                |> ToId.ofProcessId
                |> mkEnvelope pctx sid
                |> Envelope.withItem pctx.Serializer (Back (vs, last))
                |> List.ofItem
            stt, envs

        let withTknEnvelope pctx sid tkn target (stt, dest) =
            let envs = 
                dest
                |> ToId.ofProcessId
                |> mkEnvelope pctx sid
                |> Envelope.withItem pctx.Serializer (Token (tkn, target))
                |> List.ofItem
            stt, envs

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
            | Go (vs, last), Uninitialized stt -> 
                stt
                |> moveIntoChildRunning env.Fid
                |> withSuccessor        last
                |> withToBeVisited      (pctx |> ProcessCtx.procsCanSentToExceptFid env.Fid)
                |> fun stt -> 
                    match (stt, vs) with
                    | RanToCompletion stt ->
                        stt
                        |> moveIntoDone
                        |> withSendToSource env
                        |> withRouting      (ToId.ofFromId env.Fid)
                        |> withBackEnvelope pctx sid (pid::vs) pid
                        |> withMessage      "go/done"
                        |> withAsyncFlow    (doDbg "<<< DONE >>>")
                    | StillIncomplete stt ->
                        stt
                        |> withSendToNeighborExcept vs
                        |> withRouting              (ToId.ofFromId env.Fid)
                        |> withGoEnvelope           pctx sid (pid::vs) pid
                        |> withMessage              "go/go"
                        |> withAsyncFlow            aempty
            | Go _,  stt ->
                stt
                |> withNoEnvelopes
                |> withMessage      "go/err"
                |> withAsyncFlow    (doErr "We should not be here")
            | Back (vs, last), stt ->
                match stt, vs with
                | RanToCompletion stt ->
                    match stt with
                    | Root stt ->
                        stt
                        |> withSuccessor   last
                        |> withRoutingFst  (ToId.ofFromId env.Fid)
                        |> moveIntoDone
                        |> withNoEnvelopes
                        |> withMessage     "back/done"
                        |> withAsyncFlow   (doDbg "<<< DONE >>>")
                    | Child stt ->
                        stt
                        |> moveIntoDone
                        |> withSendToParent
                        |> withRouting      (ToId.ofFromId env.Fid)
                        |> withBackEnvelope pctx sid vs last
                        |> withMessage      "back/done"
                        |> withAsyncFlow    (doDbg "<<< DONE >>>")
                    | stt ->
                        stt
                        |> withNoEnvelopes
                        |> withMessage     "back/err"
                        |> withAsyncFlow   (doErr "We should not be here")
                | StillIncomplete stt ->
                    stt
                    |> withSendToNeighborExcept vs
                    |> withRouting              (ToId.ofFromId env.Fid)
                    |> withGoEnvelope           pctx sid vs last
                    |> withMessage              "back/go"
                    |> withAsyncFlow            aempty
            | Token (tkn, target), Done stt ->
                let target = 
                    if (target = pid)
                    then 
                        stt |> doDbg (sprintf "<<< CONSUME TOKEN (%d) >>>" tkn) |> Async.RunSynchronously
                        stt |> successor
                    else 
                        target

                stt
                |> moveIntoTokenized tkn
                |> withSendToRoute   env.Fid
                |> withTknEnvelope   pctx sid tkn target
                |> withMessage       "tkn/tkn"
                |> withAsyncFlow     aempty
            | Token (tkn, target), Tokenized stt ->

                stt
                |> withNoEnvelopes
                |> withMessage     "tkn/done"
                |> withAsyncFlow   (doDbg "<<< TOKEN DONE >>>")

            | Token _, _ ->
                stt
                |> withNoEnvelopes
                |> withMessage     "tkn/err"
                |> withAsyncFlow   (doErr "Not implemented!")

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
            | ApiStart, Uninitialized stt ->
                stt
                |> moveIntoRootRunning
                |> withToBeVisited (pctx |> ProcessCtx.procsCanSentTo)
                |> fun stt -> 
                    match stt, [] with
                    | RanToCompletion stt ->
                        stt
                        |> moveIntoDone
                        |> withNoEnvelopes
                        |> withMessage     "start/done"
                        |> withAsyncFlow   (doDbg "<<< DONE >>>")
                    | StillIncomplete stt ->
                        stt
                        |> withSendToNeighbor
                        |> withFirstProcess
                        |> withGoEnvelope     pctx sid [pid] pid
                        |> withMessage        "start/go" 
                        |> withAsyncFlow      aempty
            | ApiToken tkn, Done stt ->
                stt
                |> moveIntoTokenized   tkn
                |> withSendToRouteAny
                |> withTknEnvelope     pctx sid tkn pid
                |> withMessage         "tknapi/tkn"
                |> withAsyncFlow       aempty
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
        |> KFlow.map (List.map (fun (pid, p) -> (pid, RingProc p)))
        
    /// Initiates the ring creation
    let start sid (RingProc p) = 
        Kernel.apiCallWithArgs sid ApiStart p

    /// Passes the token around.
    let token sid tkn  (RingProc p) =
        Kernel.apiCallWithArgs sid (ApiToken tkn) p

