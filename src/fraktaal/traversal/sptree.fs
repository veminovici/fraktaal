namespace Simplee.DSystems.Traversals

open Simplee.DSystems

type SpanningTreeProc = SpanningTreeProc of IProcessPub

[<RequireQualifiedAccess>]
module SpanningTree =

    /// The public api messages
    type private ApiMessage =
        | ApiStart of int
        with
        override this.ToString() = 
            match this with
            | ApiStart dt -> sprintf "START (%d)" dt
            
    type private IntResult = {
        Pid: ProcessId 
        V:   int }
        with 
        override this.ToString() = sprintf "p=%O %d" this.Pid this.V

    /// The internal messages
    type private IntMessage = 
        | Go   of int
        | Back of IntResult list
        | NotYourChild

    type private SttRunning = {
        Children: ProcessId list
        Expected: ProcessId list
        Results:  IntResult list }
        with
        override this.ToString() =
            sprintf "cs=%O es=%O rs=%O" this.Children this.Expected this.Results

    [<RequireQualifiedAccess>]
    module private SttRunning = 
        let empty = { 
            Children = [] 
            Expected = [] 
            Results = [] }

        let withExpected ns r =  { r with Expected = ns }
        let withoutExpected xid r = { r with Expected = r.Expected |> List.where (fun x -> x <> xid) }

        let (|Completed|Incompleted|) r = 
            if List.isEmpty r.Expected then Completed else Incompleted

        let withResults rs r = { r with Results = rs @ r.Results }

        let withChild c r = { r with Children = c :: r.Children }

    type private SttDone = {
        Children: ProcessId list
        Results:  IntResult list }
        with
        override this.ToString() =
            sprintf "cs=%O rs=%O" this.Children this.Results

    type private State =
        | SttUninitialized
        | SttRootRunning  of SttRunning
        | SttRootDone     of SttDone
        | SttChildRunning of ProcessId * SttRunning
        | SttChildDone    of ProcessId * SttDone
        with
        override this.ToString() =
            match this with
            | SttUninitialized           -> "uninitialized"
            | SttRootRunning        ctx  -> sprintf "ROOT-RUNG %O" ctx
            | SttRootDone           ctx  -> sprintf "ROOT-DONE %O" ctx
            | SttChildRunning (pid, ctx) -> sprintf "CHLD-RUNG p=%O %O" pid ctx
            | SttChildDone    (pid, ctx) -> sprintf "CHLD-DONE p=%O %O" pid ctx

    [<RequireQualifiedAccess>]
    module private State =
        let empty = SttUninitialized

        let setRootRunning _ = 
            SttRunning.empty
            |> SttRootRunning

        let setChildRunning xid _ = 
            SttChildRunning (xid, SttRunning.empty)

        let setRootCompleted = function
            | SttRootRunning r -> { Children = r.Children; Results = r.Results } |> SttRootDone
            | stt              -> failwith (notSupported "setRootCompleted" stt)

        let setChildCompleted = function
            | SttChildRunning (xid, r) -> let r = { Children = r.Children; Results = r.Results } in SttChildDone (xid, r)
            | stt                      -> failwith (notSupported "setChildCompleted" stt)

        let withExpected ns = function
            | SttRootRunning         r -> r |> SttRunning.withExpected ns |> SttRootRunning
            | SttChildRunning (xid, r) -> let r = r |> SttRunning.withExpected ns in SttChildRunning (xid, r)
            | stt                      -> failwith (notSupported "withExpected" stt)

        let withResults rs = function
            | SttRootRunning r         -> r |> SttRunning.withResults rs |> SttRootRunning
            | SttChildRunning (xid, r) -> let r = r |> SttRunning.withResults rs in SttChildRunning (xid, r)
            | stt                      -> failwith (notSupported "withResults" stt)

        let withResult = List.ofItem >> withResults

        let withChild c = function
            | SttRootRunning r         -> r |> SttRunning.withChild c |> SttRootRunning
            | SttChildRunning (xid, r) -> let r = r |> SttRunning.withChild c in SttChildRunning (xid, r)
            | stt                      -> failwith (notSupported "withChild" stt)

        let parent = function
            | SttChildRunning (xid, _) -> xid
            | SttChildDone    (xid, _) -> xid
            | stt                      -> failwith (notSupported "parent" stt)

        let results = function
            | SttRootDone         r  -> r.Results
            | SttRootRunning      r  -> r.Results
            | SttChildDone    (_, r) -> r.Results
            | SttChildRunning (_, r) -> r.Results
            | stt                    -> failwith (notSupported "results" stt)

        let withoutExpected n = function
            | SttRootRunning r         -> r |> SttRunning.withoutExpected n |> SttRootRunning
            | SttChildRunning (xid, r) -> let r = r |> SttRunning.withoutExpected n in SttChildRunning (xid, r)
            | stt                      -> failwith (notSupported "withoutExpected" stt)

    let private (|RootCompleted|ChildCompleted|StillRunning|) (stt: State) =
        match stt with
        | SttRootRunning r       -> match r with SttRunning.Completed -> RootCompleted  stt | _ -> StillRunning stt
        | SttChildRunning (_, r) -> match r with SttRunning.Completed -> ChildCompleted stt | _ -> StillRunning stt
        | SttRootDone  _         -> RootCompleted  stt
        | SttChildDone _         -> ChildCompleted stt
        | SttUninitialized       -> failwith (notSupported "RootCompleted..." stt)

    /// Make an envelope for POSITION internal message.
    let private goEnvelope pctx sid (pos: IntMessage) dest =
        ()
        |> Envelope.ofRndEid
        |> Envelope.withSid sid
        |> Envelope.withFid (ProcessCtx.fid pctx)
        |> Envelope.withTid (Neighbor.tid dest)
        |> Envelope.withItem pctx.Serializer pos

    let private backEnvelope pctx sid rs dest =
        ()
        |> Envelope.ofRndEid
        |> Envelope.withSid sid
        |> Envelope.withFid (ProcessCtx.fid pctx)
        |> Envelope.withTid dest
        |> Envelope.withItem pctx.Serializer (Back rs)

    let private nycEnvelope pctx sid dest =
        ()
        |> Envelope.ofRndEid
        |> Envelope.withSid sid
        |> Envelope.withFid (ProcessCtx.fid pctx)
        |> Envelope.withTid dest
        |> Envelope.withItem pctx.Serializer NotYourChild

    let private envsInitialize pctx sid dt stt =
        let envs = 
            pctx
            |> ProcessCtx.canSendTo
            |> List.map (goEnvelope pctx sid (Go dt)) 

        stt, envs

    // the envelopes sent when we fwd a position.
    let private envsFwdGo pctx env stt =

        let envs =
            pctx
            |> ProcessCtx.canSendToExceptFid (Envelope.fid env)
            |> List.map (fun n -> 
                env
                |> Envelope.withRndEid
                |> Envelope.withFid (FromId.ofProcessId pctx.Pid)
                |> Envelope.withTid (Neighbor.tid n))

        stt, envs

    let private envsBack pctx sid stt =

        let envs = 
            stt
            |> State.parent
            |> ToId.ofProcessId
            |> List.ofItem
            |> List.map (backEnvelope pctx sid (State.results stt))

        stt, envs

    let private envsNotYourChild pctx sid tid stt =
        let envs = 
            tid
            |> List.ofItem
            |> List.map (nycEnvelope pctx sid)

        stt, envs

    let private rcv : ReceiveHandle = fun pstt pctx env -> 
        let pid              = pctx.Pid
        let stt : State      = ProcessState.toItem pstt
        let msg : IntMessage = Envelope.msg pctx.Serializer env
        let sid              = env.Sid

        let doTrace     = List.ofItem >> pctx.Trace
        let doRcv       = msg |> sprintf "stt=[%O] msg=[%O]" stt |> Trace.prcv pid env |> doTrace
        let doSnd m e = sprintf "%s stt=[%O]" m >> Trace.psnd pid e   >> doTrace
        let doErr m   = sprintf "%s stt=[%O]" m >> Trace.perr pid sid >> doTrace
        let doDbg m   = sprintf "%s stt=[%O]" m >> Trace.pdbg pid sid >> doTrace

        let continueGo dt = function
            | StillRunning stt -> 
                async {
                return
                    stt
                    |> envsFwdGo pctx env
                    |> withMessage "go/go" }
            | ChildCompleted stt ->
                async {
                let stt, envs = 
                    stt
                    |> State.withResult { Pid = pid; V = dt }
                    |> envsBack pctx sid

                do! doDbg "<<< DONE >>>" stt

                return stt, envs, "go/back" }
            | stt -> 
                async {
                let m = "go/error"
                do! doErr m stt
                return stt, [], m }

        let continueBack m = function
            | RootCompleted stt  ->
                async {                    
                let stt, envs, msg = 
                    stt
                    |> State.withResult { Pid = pid; V = 0 }
                    |> State.setRootCompleted
                    |> withNoEnvelopes
                    |> withMessage (sprintf "%s/done" m) 
                
                do! doDbg "<<< DONE >>>" stt

                return stt, envs, msg }
            | ChildCompleted stt ->
                async {
                let stt, envs, msg =
                    stt
                    |> State.withResult { Pid = pid; V = 0 }
                    |> State.setChildCompleted
                    |> envsBack pctx sid
                    |> withMessage (sprintf "%s/back" m) 
                    
                do! doDbg "<<< DONE >>>" stt

                return stt, envs, msg }
            | StillRunning stt   ->
                async {
                return 
                    stt
                    |> withNoEnvelopes
                    |> withMessage (sprintf "%s/waiting" m) }

        async {
        do! doRcv

        let! stt, envs, msg =
            match msg with
            | Go dt ->
                match stt with
                | SttUninitialized ->
                    stt
                    |> State.setChildRunning (ProcessId.ofFromId env.Fid)
                    |> State.withExpected (ProcessCtx.procsCanSentToExceptFid env.Fid pctx)
                    |> continueGo dt
                | _ ->
                    async {
                    return 
                        stt
                        |> envsNotYourChild pctx sid (ToId.ofFromId env.Fid)
                        |> withMessage "go/notYourChild" }
            | Back rs ->
                let fid = ProcessId.ofFromId env.Fid in
                stt
                |> State.withoutExpected fid
                |> State.withChild fid
                |> State.withResults rs
                |> continueBack "back-res"

            | NotYourChild ->
                let fid = ProcessId.ofFromId env.Fid in
                stt
                |> State.withoutExpected fid
                |> continueBack "back-nil"

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

        let continueStart dt = function
        | StillRunning stt -> 
            async {
            return
                stt
                |> envsInitialize pctx sid dt
                |> withMessage "api/go" }
        | RootCompleted stt ->
            async {
            
            let stt, envs, msg =  
                stt 
                |> State.setRootCompleted
                |> withNoEnvelopes
                |> withMessage "api/done" 

            do! doDbg "<<< DONE >>>" stt
            return stt, envs, msg }
        | stt -> 
            async {
            let m = "api/error"
            do! doErr m stt
            return stt, [], m }

        async {
        do! doApi

        let! stt, envs, msg =
            match api with
            | ApiStart dt -> 
                    stt
                    |> State.setRootRunning
                    |> State.withExpected (pctx |> ProcessCtx.procsCanSentTo)
                    |> continueStart dt

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
        |> KFlow.map (List.map (fun (pid, p) -> (pid, SpanningTreeProc p)))

    /// Initiates a learning session, with a given
    /// session identifier and starting from a given process.
    let start sid dt (SpanningTreeProc p) = 
        Kernel.apiCallWithArgs sid (ApiStart dt) p