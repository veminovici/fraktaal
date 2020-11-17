namespace Simplee.DSystems

[<AutoOpen>]
module Utils =

    type Comparison =
        | ALessThanB
        | AEqualToB
        | AGreaterThanB

    type Compare<'T> = 'T -> 'T -> Comparison

/// Extenstions to Lists.
[<RequireQualifiedAccessAttribute>]
module List =

    /// Creates a list with one item.
    let ofItem x = [x]

    /// Determines if the ys is a sublist of xs
    let sublistOf xs ys =

        let rec loop ys = 
            match ys with
            | [] -> true
            | y :: tail when List.contains y xs -> loop tail
            | _ -> false

        loop ys

    /// Given a list, concats the item representations.
    let toStringF s f xs =
        xs
        |> List.map f
        |> String.concat s

    /// Given a list, concats the item representations
    /// each of them on a new line.
    let toString xs = toStringF "\n" (sprintf "%O") xs

    /// Given a list, gets its string representation
    /// preappended with a given title.
    let toStringWithTtlF s f t xs =
        t 
        :: (xs |> List.map f)
        |> String.concat s

    /// A string rerpesentation for a list, each item represented
    /// on a new line with a given title.
    let toStringWithTtl t xs = toStringWithTtlF "\n" (sprintf "%O") t xs

/// Extensions to Async.
[<RequireQualifiedAccess>]
module Async =

    let sleep (ms: int) a = async.Bind (a, fun r -> async {
        do! Async.Sleep ms
        return r })

    /// Maps the result of an async operation, using a given function.
    let map f a = async.Bind (a, f >> async.Return)

    /// Apply the function result of an async operation
    /// the to the result of a second async operation.
    let apply f m = async {
        let! f = f
        let! m = m
        return f m }

//
// Identifiers
//

type ProcessId  = private ProcessId  of string
    with 
    override this.ToString() = let (ProcessId pid) = this in pid
type FromId     = private FromId     of string
    with 
    override this.ToString() = let (FromId fid) = this in fid
type ToId       = private ToId       of string
    with 
    override this.ToString() = let (ToId tid) = this in tid
type SessionId  = private SessionId  of string
    with 
    override this.ToString() = let (SessionId sid) = this in sid
type EnvelopeId = private EnvelopeId of string
    with 
    override this.ToString() = let (EnvelopeId eid) = this in eid

[<RequireQualifiedAccess>]
module ProcessId = 
    let ofStr = ProcessId
    let empty = ofStr ""

    let pid (ProcessId p) = p

    let toFromId (ProcessId pid) = FromId pid
    let ofFromId (FromId    fid) = ProcessId fid

    let toToId   (ProcessId pid) = ToId   pid
    let ofToId   (ToId      tid) = ProcessId tid

[<RequireQualifiedAccess>]
module FromId =
    let ofStr = FromId
    let empty = ofStr ""

    let fid (FromId f) = f

    let internal kid = ofStr "_krnl_fid_"

    let toProcessId (FromId fid)    = ProcessId fid
    let ofProcessId (ProcessId pid) = FromId pid

    let toToId      (FromId fid)    = ToId fid
    let ofToId      (ToId tid)      = FromId tid

[<RequireQualifiedAccess>]
module ToId =
    let ofStr = ToId
    let empty = ofStr ""

    let tid (ToId t) = t

    let internal kid = ofStr "_krnl_tid_"

    let toProcessId (ToId tid)      = ProcessId tid
    let ofProcessId (ProcessId pid) = ToId pid

    let toFromId    (ToId tid)      = FromId tid
    let ofFromId    (FromId fid)    = ToId fid

[<RequireQualifiedAccess>]
module SessionId =
    let ofStr = SessionId
    let empty = ofStr ""

    let sid (SessionId s) = s

    let internal kid = ofStr "_krnl_sid_"

[<RequireQualifiedAccess>]
module EnvelopeId =
    let ofStr = EnvelopeId
    let empty = ofStr ""

    let eid (EnvelopeId e) = e

    let rnd () =
        let g = System.Guid.NewGuid()
        g.ToString() |> ofStr

    let internal kid = ofStr "_krnl_eid_"

//
// Serializer
//

type ISerializer =
    abstract Serialize:   'a -> byte []
    abstract Deserialize: byte [] -> 'a

//
// Payload
//

type Payload = 
    | Payload     of byte[]
    | PingPayload
    with 
    override this.ToString() = 
        match this with
        | Payload xs  -> sprintf "%dbs" (Array.length xs)
        | PingPayload -> "ping"

[<RequireQualifiedAccess>]
module Payload =
    let ofBytes = Payload
    let empty   = ofBytes Array.empty

    let internal pingPld = PingPayload //ofBytes [| 0xFuy; 0x0uy; 0x0uy; 0xBuy; 0xAuy; 0xAuy |]

    let ofItem<'a> (s: ISerializer) (a: 'a) = a |> s.Serialize |> ofBytes
    let toItem<'a> (s: ISerializer) pld : 'a = 
        match pld with
        | Payload bs  -> bs |> s.Deserialize
        | PingPayload -> failwithf "toItem cannot be applied to %O" pld

//
// Envelope
//

type Envelope = {
    Eid: EnvelopeId
    Sid: SessionId
    Fid: FromId
    Tid: ToId
    Pld: Payload }
    with 
    override this.ToString() = 
        let eid = EnvelopeId.eid this.Eid
        sprintf "%O->%O Sid=%O pld=%O Eid=%O..." this.Fid this.Tid this.Sid this.Pld eid.[..2]

[<RequireQualifiedAccess>]
module Envelope =
    let empty = { 
        Eid = EnvelopeId.empty
        Sid = SessionId.empty
        Fid = FromId.empty
        Tid = ToId.empty
        Pld = Payload.empty }

    let eid env = env.Eid
    let sid env = env.Sid
    let fid env = env.Fid
    let tid env = env.Tid

    let internal pld env = env.Pld

    let msg srzl (env: Envelope) = env |> pld |> Payload.toItem srzl

    let withEid  eid env = { env with Eid = eid }
    let withSid  sid env = { env with Sid = sid }
    let withFid  fid env = { env with Fid = fid }
    let withTid  tid env = { env with Tid = tid }
    let withPld  pld env = { env with Pld = pld }
    let withItem s i env = let pld = Payload.ofItem s i in withPld pld env

    let ofEid eid = empty |> withEid eid 

    let ofRndEid    () = () |> EnvelopeId.rnd |> ofEid
    let withRndEid env = let eid = EnvelopeId.rnd () in env |> withEid eid

    let internal ping tid = 
        EnvelopeId.kid
        |> ofEid 
        |> withFid FromId.kid
        |> withTid (ToId.ofProcessId tid)
        |> withSid SessionId.kid
        |> withPld Payload.pingPld

//
// Links and neighbors
//

type Weight =
    | NoWeight
    | Weight of float
    with 
    override this.ToString() =
        match this with
        | NoWeight -> "w-no"
        | Weight f -> sprintf "w-%.2f" f
type LinkData = private {
    Fid: ProcessId
    Tid: ProcessId
    W:   Weight }
type Link =
    | OneWay  of LinkData
    | TwoWays of LinkData

[<RequireQualifiedAccess>]
module LinkData =
    let empty = { 
        Fid = ProcessId.empty
        Tid = ProcessId.empty
        W = NoWeight }

[<RequireQualifiedAccess>]
module Link =
    //
    // Constructors
    //

    let emptyBi  = TwoWays LinkData.empty
    let emptyUni = OneWay  LinkData.empty

    let withFid fid = function
        | TwoWays ld -> TwoWays { ld with Fid = fid }
        | OneWay  ld -> OneWay  { ld with Fid = fid }

    let withTid tid = function
        | TwoWays ld -> TwoWays { ld with Tid = tid }
        | OneWay  ld -> OneWay  { ld with Tid = tid }

    let withWeight w = function
        | TwoWays ld -> TwoWays { ld with W = w }
        | OneWay  ld -> OneWay   { ld with W = w }

    let bilinkWithWeight w fid tid = 
        emptyBi
        |> withFid fid
        |> withTid tid
        |> withWeight w

    let bilink = bilinkWithWeight NoWeight

    let unilinkWithWeight w fid tid =
        emptyUni
        |> withFid fid
        |> withTid tid
        |> withWeight w

    let unilink = unilinkWithWeight NoWeight

    module Ops =
        let (=>>) x y = unilink x y
        let (<=>) x y = bilink  x y
        let (<<=) x y = unilink y x
        let (..>)  l w = withWeight (Weight w) l

type Neighbor =
    | Sourcer           of ProcessId * Weight
    | Receiver          of ProcessId * Weight
    | SourcerOrReceiver of ProcessId * Weight

[<RequireQualifiedAccess>]
module Neighbor =
    let toString pid = function
        | Sourcer           (f, w) -> sprintf "%O <<= %O (%O)" pid f w
        | Receiver          (t, w) -> sprintf "%O =>> %O (%O)" pid t w
        | SourcerOrReceiver (e, w) -> sprintf "%O <=> %O (%O)" pid e w

    let toStringWithTtlF s t pid ns =
        List.toStringWithTtlF s (toString pid) t ns

    let toStringWithTtl t pid ns = toStringWithTtlF "\n" t pid ns

    let private startOfLink = function
        | OneWay  l -> FromId.ofProcessId l.Fid, Receiver (l.Tid, l.W)
        | TwoWays l -> FromId.ofProcessId l.Fid, SourcerOrReceiver (l.Tid, l.W) 

    let private endOfLink = function
        | OneWay  l -> ToId.ofProcessId l.Tid, Sourcer (l.Fid, l.W)
        | TwoWays l -> ToId.ofProcessId l.Tid, SourcerOrReceiver (l.Fid, l.W)

    let endsOfLink l =
        startOfLink l, endOfLink l

    let pid = function
        | Sourcer  (pid, _)          -> pid
        | Receiver (pid, _)          -> pid
        | SourcerOrReceiver (pid, _) -> pid

    let tid = pid >> ToId.ofProcessId
    let fid = pid >> FromId.ofProcessId

    let (|CanSendTo|_|) = function
        | Sourcer           _      -> None
        | Receiver          (p, w)
        | SourcerOrReceiver (p, w) -> Some (p, w)

    let (|CanReceiveFrom|_|) = function
        | Receiver          _      -> None
        | Sourcer           (p, w)
        | SourcerOrReceiver (p, w) -> Some (p, w)

    let canSendTo      = function CanSendTo _      -> true | _ -> false
    let canReceiveFrom = function CanReceiveFrom _ -> true | _ -> false

//
// API arguments
//

type ApiArguments = private ApiArguments of obj

[<RequireQualifiedAccess>]
module ApiArguments =
    let ofItem a = a |> box |> ApiArguments
    let toItem (ApiArguments args) = args |> unbox

//
// Tracing and logging
//

type TrcConfiguration =
    | TrcCMakeProcess   of ProcessId
    | TrcCAddNeighbors  of ProcessId * Neighbor list
    with
    override this.ToString() =
        match this with
        | TrcCMakeProcess pid        -> sprintf "MKPRC pid=[%O]" pid
        | TrcCAddNeighbors (pid, ns) -> sprintf "ADDNS pid=[%O] ns=%O" pid ns
type TrcSystem =
    | TrcSDebug   of string
    | TrcSInfo    of string
    | TrcSWarning of string
    | TrcSError   of string
    with
    override this.ToString() =
        match this with
        | TrcSDebug   msg -> sprintf "DEBUG %s" msg
        | TrcSInfo    msg -> sprintf "INFOR %s" msg
        | TrcSWarning msg -> sprintf "WARNG %s" msg
        | TrcSError   msg -> sprintf "ERROR %s" msg
type TrcKernel =
    | TrcKDispatching of Envelope list
    | TrcKDelivered   of ProcessId * Envelope
    | TrcKApiCall     of ProcessId * SessionId * ApiArguments
    | TrcKSystem      of TrcSystem
    with
    override this.ToString() =
        match this with
        | TrcKDispatching  envs        -> 
            match envs with
            | []   ->         "DISPG nil"
            | [e]  -> sprintf "DISPG (%d) %O" 1 e
            | envs -> sprintf "DISPG (%d)\n%s" (List.length envs) (List.toString envs)
        | TrcKDelivered    (pid, env)  -> sprintf "DLVRD pid=[%O] env=[%O]" pid env
        | TrcKApiCall (pid, sid, args) -> sprintf "APICL pid=[%O] sid=[%O] args=[%O]" pid sid args
        | TrcKSystem s                 -> sprintf "%O" s
type TrcProcess =
    | TrcPApiCall  of ProcessId * SessionId * string
    | TrcPReceived of ProcessId * Envelope  * string
    | TrcPSending  of ProcessId * Envelope list * string
    | TrcPDebug    of ProcessId * SessionId * string
    | TrcPError    of ProcessId * SessionId * string
    with
    override this.ToString() =
        match this with
        | TrcPApiCall  (pid, sid,  msg) -> sprintf "APICL pid=[%O] sid=[%O] %s"      pid sid msg
        | TrcPReceived (pid, env,  msg) -> sprintf "RECVD pid=[%O] env=[%O] %s" pid env msg
        | TrcPSending  (pid, envs, msg) -> 
            match envs with
            | []   -> sprintf "SENDG pid=[%O] no envelopes %s" pid msg
            | [x]  -> sprintf "SENDG pid=[%O] %s (1) env=[%O]" pid msg x
            | envs -> sprintf "SENDG pid=[%O] %s (%d)\n%s"     pid msg (List.length envs) (List.toString envs)
        | TrcPDebug    (pid, sid,  msg) -> sprintf "DEBUG pid=[%O] sid=[%O] %s"    pid sid msg
        | TrcPError    (pid, sid,  msg) -> sprintf "ERROR pid=[%O] sid=[%O] %s"    pid sid msg
type TrcEntry =
    | TrcEntryCfg  of TrcConfiguration
    | TrcEntryKrnl of TrcKernel
    | TrcEntryProc of TrcProcess
    with
    override this.ToString() =
        match this with
        | TrcEntryCfg  e -> sprintf "CNFG %O"  e
        | TrcEntryKrnl e -> sprintf "KRNL %O" e
        | TrcEntryProc e -> sprintf "PROC %O" e
type ITracerInt  =
    abstract Trace: TrcEntry list -> Async<unit>

[<RequireQualifiedAccess>]
module Trace =

    // Cofiguration tracing

    let makeProcess  = TrcCMakeProcess  >> TrcEntryCfg
    let addNeighbors = TrcCAddNeighbors >> TrcEntryCfg

    // Kernel level tracing

    let dispatching  = TrcKDispatching >> TrcEntryKrnl
    let delivered    = TrcKDelivered   >> TrcEntryKrnl
    let apicalled    = TrcKApiCall     >> TrcEntryKrnl

    let slog         = TrcKSystem >> TrcEntryKrnl
    let debug        = TrcSDebug   >> slog
    let info         = TrcSInfo    >> slog
    let warn         = TrcSWarning >> slog
    let err          = TrcSError   >> slog

    // Process level tracing

    let papi p s  m  = (p, s, m)  |> TrcPApiCall  |> TrcEntryProc
    let prcv p e  m  = (p, e, m)  |> TrcPReceived |> TrcEntryProc
    let psnd p es m  = (p, es, m) |> TrcPSending  |> TrcEntryProc
    let pdbg p s  m  = (p, s, m)  |> TrcPDebug    |> TrcEntryProc
    let perr p s  m  = (p, s, m)  |> TrcPError    |> TrcEntryProc

    //
    let pid = function
        | TrcPApiCall  (pid, _, _) -> pid
        | TrcPReceived (pid, _, _) -> pid
        | TrcPSending  (pid, _, _) -> pid
        | TrcPDebug    (pid, _, _) -> pid
        | TrcPError    (pid, _, _) -> pid

//
// Logs
//

type LogEntry = { 
    Ticks: int64; 
    Entry: TrcEntry }
    with
    override this.ToString() =
        let dt = System.DateTime(this.Ticks)

        let tm = System.DateTime(this.Ticks).ToLongTimeString()
        let ms = dt.Millisecond

        sprintf "%s (%03d): %O" tm ms this.Entry
type ILoggerPub =
    abstract Logs: Async<LogEntry list>

[<RequireQualifiedAccess>]
module LogEntry =
    let (|ProccessLogEntry|CfgLogEntry|KrnlLogEntry|) (e: LogEntry) = 
        match e.Entry with
        | TrcEntryCfg  c -> CfgLogEntry  c
        | TrcEntryKrnl k -> KrnlLogEntry k
        | TrcEntryProc p -> ProccessLogEntry p

    let isProcLog = function ProccessLogEntry _ -> true | _ -> false
    let isKrnlLog = function KrnlLogEntry     _ -> true | _ -> false
    let isCfgLog  = function CfgLogEntry      _ -> true | _ -> false

    let (|ProcessDbgLogEntry|_|) (e: LogEntry) =
        match e with
        | ProccessLogEntry (TrcPDebug _) -> Some e
        | _ -> None 

    let isPDbgLog = function ProcessDbgLogEntry _ -> true | _ -> false

    let (|LogFromProcess|_|) pid le = 
        match le with
        | ProccessLogEntry p when Trace.pid p = pid -> Some le
        | _                                -> None

    let forProc pid = function
        | LogFromProcess pid _ -> true
        | _                   -> false

[<RequireQualifiedAccess>]
module Logger =

    type private Message =
        | MTrace of TrcEntry list
        | MLogs  of AsyncReplyChannel<LogEntry list>

    let private toLogEntry t = { 
        Ticks = System.DateTime.UtcNow.Ticks
        Entry = t }

    let create () =
        let mbox = MailboxProcessor.Start(fun inbox ->
        
            let rec loop stt = async {
                match! inbox.Receive() with
                | MTrace ts ->
                    return! 
                        ts
                        |> List.map toLogEntry
                        |> (@) stt
                        |> loop
                | MLogs channel ->
                    channel.Reply(stt)
                    return! loop stt }
            
            loop [])

        let lapi = { new ITracerInt with
            member _.Trace ts = mbox.Post (MTrace ts) |> async.Return }

        let klog = { new ILoggerPub with
            member _.Logs = mbox.PostAndAsyncReply MLogs }

        klog, lapi.Trace

//
// Process state and the api result
//

type ProcessState = private ProcessState of obj
type ApiResult    = private ApiResult of obj

[<RequireQualifiedAccess>]
module ProcessState =
    let ofItem a = a |> box |> ProcessState
    let toItem (ProcessState a) = unbox a

    let nil = ofItem ()

[<RequireQualifiedAccess>]
module ApiResult = 
    let ofItem a = a |> box |> ApiResult

    let nil = ofItem ()

//
// The context for the calls to process implementation.
//

type ProcessCtx  = {
    Pid:        ProcessId
    Neighbors:  Neighbor list
    Serializer: ISerializer
    Trace:      TrcEntry list -> Async<unit> }

module ProcessCtx =

    let canSendTo (pctx: ProcessCtx) =
        pctx.Neighbors
        |> List.where Neighbor.canSendTo

    let procsCanSentTo = 
        canSendTo 
        >> List.map Neighbor.pid

    let canSendToExceptFid f p =
        p
        |> canSendTo
        |> List.where (fun n -> Neighbor.fid n <> f)

    let procsCanSentToExceptFid f = 
        canSendToExceptFid f
        >> List.map Neighbor.pid

    let canReceiveFrom (pctx: ProcessCtx) =
        pctx.Neighbors
        |> List.where Neighbor.canReceiveFrom

    let procsCanReceiveFrom =
        canReceiveFrom 
        >> List.map Neighbor.pid

    let upstream pctx = 
        pctx
        |> procsCanReceiveFrom
        |> function
        | []  -> failwithf "Process does not have an upstream neighbor (%O)" pctx.Pid
        | [x] -> x
        | _   -> failwithf "Process has too many upstream neighbors (%O)" pctx.Pid

    let downstream pctx = 
        pctx
        |> procsCanSentTo
        |> function
        | []  -> failwithf "Process does not have a downstream neighbor (%O)" pctx.Pid
        | [x] -> x
        | _   -> failwithf "Process has too many downstream neighbors (%O)" pctx.Pid

    let pid (p: ProcessCtx) = p.Pid
    let fid                 = pid >> FromId.ofProcessId
    let tid                 = pid >> ToId.ofProcessId

type ApiCallHandle = ProcessState -> ProcessCtx -> SessionId -> ApiArguments -> Async<ProcessState * Envelope list * ApiResult>
type ReceiveHandle = ProcessState -> ProcessCtx -> Envelope  -> Async<ProcessState * Envelope list>
type ConfigHandle  = ProcessState -> ProcessCtx -> Neighbor list -> Async<ProcessState * Envelope list>

type IProcessPub  =
    abstract Id:         Async<ProcessId>       // process identifier
    abstract Neighbors:  Async<Neighbor list>   // list of neighbors 
    abstract ProcState:  Async<ProcessState>    // process internal state
    abstract Serializer: Async<ISerializer>     // serializer
    abstract ApiCall:    SessionId * ApiArguments -> Async<ApiResult>   // call public api
type IKernelPub   =
    abstract Logger:   Async<ILoggerPub>           // logger
    abstract Ping:     ProcessId -> Async<unit>    // sends a ping to a given
    abstract SLog:     TrcSystem -> Async<unit>    // logs into the system log
    abstract AddLinks: Link list -> Async<unit>    // add links between procs 
    abstract Spawn:    ProcessCfg list -> Async<(ProcessId * IProcessPub) list> // spawn processes.
and  ProcessCfg   = {
    Pid: ProcessId
    Cfg: ConfigHandle
    Rcv: ReceiveHandle
    Api: ApiCallHandle
    Zro: ProcessState }

[<RequireQualifiedAccess>]
module ProcessCfg =

    let empty pid = {
        Pid = pid
        Cfg = fun ustt _ _   -> async { return ustt, [] }
        Rcv = fun ustt _ _   -> async { return ustt, [] }
        Api = fun ustt _ _ _ -> async { return ustt, [], ApiResult.nil }
        Zro = ProcessState.nil }

    let make pid = empty pid

    let withCfg fn cfg = { cfg with Cfg = fn }
    let withRcv fn cfg = { cfg with Rcv = fn }
    let withApi fn cfg = { cfg with Api = fn }
    let withZero z cfg = { cfg with Zro = ProcessState.ofItem z }

//
// Computation expressions for kernel operations.
//

type KFlow<'a> = IKernelPub -> Async<'a>

[<RequireQualifiedAccess>]
module KFlow =

    let rtrn x : KFlow<'a> = fun _ -> async.Return x

    let bind (f:'a -> KFlow<'b>) (m: KFlow<'a>) : KFlow<'b> = fun k -> async {
        let! r = m k
        return! (f r) k }

    let map (f:'a -> 'b) (c: KFlow<'a>) = bind (f >> rtrn) c

    let apply (f: KFlow<'a -> 'b>) (m: KFlow<'a>) : KFlow<'b> = fun k -> async {
        let! fn = f k
        let! a  = m k

        let flow = rtrn (fn a)
        return! flow k }

    let internal (<!>) m f = map   f m
    let internal (>>=) m f = bind  f m
    let internal (<*>) f m = apply f m

    let traverseA f list =
        let cons head tail = head :: tail

        let zro = rtrn []
        let folder h tail =
            rtrn cons <*> f h <*> tail

        List.foldBack folder list zro

    let traverseM f list =
        let cons head tail = head :: tail

        let zro = rtrn []
        let folder h tail =
            f h >>= (fun h ->
            tail >>= (fun t ->
            rtrn (cons h t)))

        List.foldBack folder list zro

    let sequenceA list = traverseA id list
    
    let sequenceM list = traverseM id list

module KFlowOps = 
    let (<!>) = KFlow.(<!>)
    let (>>=) = KFlow.(>>=)
    let (<*>) = KFlow.(<*>)

module KFlowBuilder =
    type KBuilder () =
        member _.Zero ()          = KFlow.rtrn ()
        member _.Return x         = KFlow.rtrn x
        member _.ReturnFrom x     = x
        member _.Bind    (m, f)   = KFlow.bind f m
        member _.Combine (p1, p2) = KFlow.bind (fun _ -> p2) p1

    let kernel = KBuilder()

//
// The kernel functionality
//

module Kernel =

    open MBrace.FsPickler

    type private IProcessInt =
        abstract Receive: Envelope -> Async<Envelope list>

    type private IProcessCfg =
        abstract AddNeighbors: Neighbor list -> Async<unit>

    type private IKernelInt =
        abstract Deliver: Envelope list -> Async<unit>

    type private Pickler() =
        let bser = FsPickler.CreateBinarySerializer()

        interface ISerializer with
            member _.Serialize a    = bser.Pickle a
            member _.Deserialize bs = bser.UnPickle bs

    type private KProcess = {
        Pid: ProcessId
        Cfg: IProcessCfg
        Int: IProcessInt }
        with
        override this.ToString() = sprintf "pid=[%O]" this.Pid

    type private State = {
        Processes: KProcess list
        Links:     Link list }

    [<RequireQualifiedAccess>]
    module private State =
        let empty = { Processes = []; Links = [] }

    type private KMessage =
        | KDeliver   of Envelope list
        | KMakeProcs of ProcessCfg list * AsyncReplyChannel<(ProcessId * IProcessPub) list>
        | KAddLinks  of Link list
        | KPing      of ProcessId

    type private PState = {
        Neighbors: Neighbor list
        ConfigFn:  ConfigHandle
        ReceiveFn: ReceiveHandle
        ApiCallFn: ApiCallHandle
        ProcState: ProcessState }

    type private PMessage =
        | PReceive      of Envelope * AsyncReplyChannel<Envelope list>
        | PAddNeighbors of Neighbor list
        | PNeighbors    of AsyncReplyChannel<Neighbor list>
        | PProcState    of AsyncReplyChannel<ProcessState>
        | PApiCall      of SessionId * ApiArguments * AsyncReplyChannel<ApiResult>

    let create () = 
        let serializer   = Pickler() :> ISerializer
        let mutable kint = Unchecked.defaultof<IKernelInt>
        let lgr, trc     = Logger.create ()

        let tMakeProcess  pid     = pid        |> Trace.makeProcess  |> List.ofItem |> trc
        let tAddNeighbors pid ns  = (pid, ns)  |> Trace.addNeighbors |> List.ofItem |> trc

        let tDispatching   envs    = envs            |> Trace.dispatching |> List.ofItem |> trc
        let tDelivered     pid env = (pid, env)      |> Trace.delivered   |> List.ofItem |> trc
        let tApiCalled pid sid msg = (pid, sid, msg) |> Trace.apicalled   |> List.ofItem |> trc

        let mkProcess (pcfg: ProcessCfg) =
            let mutable ppub = Unchecked.defaultof<IProcessPub>
            let pid = pcfg.Pid
            let pctx = { Pid = pid; Neighbors = []; Serializer = serializer; Trace = trc }

            let pbox = MailboxProcessor.Start(fun inbox ->
                let rec loop stt = async {
                    let pctx = { pctx with Neighbors = stt.Neighbors }

                    match! inbox.Receive() with
                    | PReceive (env, ret) -> 
                        // The kernel has delivered us the envelope.
                        do! tDelivered pid env

                        let! pstt, envs = stt.ReceiveFn stt.ProcState pctx env
                        ret.Reply(envs)
                        return! loop { stt with ProcState = pstt }

                    | PApiCall (sid, args, ret) ->
                        // The user has called our public api
                        do! tApiCalled pid sid args

                        let! (pstt, envs, res) = stt.ApiCallFn stt.ProcState pctx sid args
                        ret.Reply(res)
                        do! kint.Deliver envs
                        
                        return! loop { stt with ProcState = pstt }

                    | PAddNeighbors ns -> 
                        // The kernel signaled that we have new links
                        do! tAddNeighbors pid ns
                        
                        let! (pstt, envs) = stt.ConfigFn stt.ProcState pctx ns
                        do! kint.Deliver envs

                        return! loop <| { stt with ProcState = pstt; Neighbors = ns @ stt.Neighbors }

                    | PNeighbors ret ->
                        ret.Reply(stt.Neighbors)
                        return! loop stt

                    | PProcState ret ->
                        ret.Reply(stt.ProcState)
                        return! loop stt
                }

                loop {
                    Neighbors = []
                    ConfigFn  = pcfg.Cfg
                    ReceiveFn = pcfg.Rcv
                    ApiCallFn = pcfg.Api
                    ProcState = pcfg.Zro })

            ppub <- { new IProcessPub with
                member _.Id                  = async.Return pcfg.Pid 
                member _.Neighbors           = pbox.PostAndAsyncReply(PNeighbors)
                member _.ProcState           = pbox.PostAndAsyncReply(PProcState)
                member _.Serializer          = async.Return serializer
                member _.ApiCall (sid, args) = pbox.PostAndAsyncReply(fun ret -> PApiCall (sid, args, ret)) }

            let pcfg = { new IProcessCfg with
                member _.AddNeighbors ns = async.Return <| pbox.Post (PAddNeighbors ns) }

            let pint = { new IProcessInt with
                member _.Receive env = pbox.PostAndAsyncReply (fun res -> PReceive (env, res)) }

            ppub, pcfg, pint

        let mkKProcess (cfg: ProcessCfg) = async {
            do! tMakeProcess cfg.Pid
            let ppub, pcfg, pint = mkProcess cfg

            let kproc =  { Pid = cfg.Pid; Cfg = pcfg; Int = pint }
            return kproc, (cfg.Pid, ppub) }

        let addNeighbor (stt: State) (pid, n) =
            stt.Processes
            |> List.where (fun kp -> kp.Pid = pid)
            |> List.map   (fun kp -> n |> List.ofItem |> kp.Cfg.AddNeighbors)
            |> Async.Parallel
            |> Async.map ignore

        let addLink stt (lnk: Link) = async {
            let (neighborOfStartProcess, neighborOfEndProcess) = Neighbor.endsOfLink lnk

            let ns = (fst neighborOfStartProcess |> ProcessId.ofFromId, snd neighborOfStartProcess)
            let ne = (fst neighborOfEndProcess   |> ProcessId.ofToId,   snd neighborOfEndProcess)

            do! addNeighbor stt ns
            do! addNeighbor stt ne
            return () }

        let sendEnv stt (env: Envelope) = 
            stt.Processes
            |> List.where (fun p -> p.Pid = ProcessId.ofToId env.Tid)
            |> List.map (fun p -> env |> p.Int.Receive)
            |> Async.Parallel
            |> Async.map (List.ofArray >> List.concat)

        let sendEnvs stt envs = async {
            do! tDispatching envs

            return! 
                envs
                |> List.map (sendEnv stt)
                |> Async.Parallel
                |> Async.map (List.ofArray >> List.concat) }

        let mbox = MailboxProcessor.Start(fun inbox ->
        
            let rec loop stt = async {
                match! inbox.Receive() with
                | KDeliver [] ->
                    return! loop stt
                | KDeliver envs ->
                    match! sendEnvs stt envs with
                    | []   -> ()
                    | envs -> do! kint.Deliver envs

                    return! loop stt
                | KMakeProcs (ps, reply) ->                    
                    let! (kprocs, procs) = 
                        ps 
                        |> List.map mkKProcess 
                        |> Async.Parallel 
                        |> Async.map (List.ofArray >> List.unzip)

                    reply.Reply procs

                    return! loop { stt with Processes = kprocs @ stt.Processes }
                | KAddLinks ls ->
                    let! _ = 
                        ls 
                        |> List.map (addLink stt) 
                        |> Async.Parallel

                    return! loop { stt with Links = ls @ stt.Links }
                | KPing tid ->

                    let! _ = 
                        tid 
                        |> Envelope.ping 
                        |> List.ofItem 
                        |> sendEnvs stt
                    
                    return! loop stt
            }
            
            loop State.empty)

        kint <- { new IKernelInt with 
            member _.Deliver (envs) = async.Return <| mbox.Post (KDeliver envs) }

        { new IKernelPub with
            member _.Logger      = async.Return <| lgr
            member _.AddLinks ls = async.Return <| mbox.Post (KAddLinks ls)
            member _.Ping    tid = async.Return <| mbox.Post (KPing tid) 
            member _.Spawn    ps = mbox.PostAndAsyncReply (fun c -> KMakeProcs(ps, c))
            member _.SLog      s = s |> Trace.slog |> List.ofItem |> trc }

    let runWithKernel     k (flow: KFlow<'a>) = flow k
    let runSyncWithKernel k (flow: KFlow<'a>) = runWithKernel k flow |> Async.RunSynchronously

    let run     (flow: KFlow<'a>) = runWithKernel (create ()) flow
    let runSync (flow: KFlow<'a>) = flow |> run |> Async.RunSynchronously

    /// add a delay of given miliseconds.
    let sleep   (ms: int) = fun (_: IKernelPub) -> Async.Sleep ms

    /// spawn a new process.
    let spawn pcfg = fun (k: IKernelPub) -> 
        pcfg
        |> List.ofItem
        |> k.Spawn
        |> Async.map List.head

    /// span several processes in parallel.
    let spawns ps = fun (k: IKernelPub) ->
        ps
        |> k.Spawn

    /// add a links between processes.
    let addLinks lnks = fun (k: IKernelPub) ->
        lnks
        |> k.AddLinks

    /// return the logs.
    let logs : KFlow<_> = fun (k: IKernelPub) -> async {
        let! lgr = k.Logger
        return! lgr.Logs }

    /// initiate an api call to a given process.
    let private _apiCall sid args (p: IProcessPub) = fun (_: IKernelPub) -> async {
        return! p.ApiCall(sid, args) }

    /// initiate an api call with arguments.TrcPDebug
    let apiCallWithArgs sid args proc = 
        _apiCall sid (ApiArguments.ofItem args) proc

    /// initiate an api call without arguments.
    let apiCallNoArgs sid proc = 
        _apiCall sid (ApiArguments.ofItem ()) proc

    /// send a ping to a given process.
    let ping tid = fun (k: IKernelPub) -> k.Ping tid

    /// return the list of neighbors for a given process.
    let neighbors (p: IProcessPub) = fun (_: IKernelPub) -> p.Neighbors

    //
    // Tracing functions.
    //

    let dbg  msg = fun (k: IKernelPub) -> msg |> TrcSDebug   |> k.SLog
    let info msg = fun (k: IKernelPub) -> msg |> TrcSInfo    |> k.SLog
    let warn msg = fun (k: IKernelPub) -> msg |> TrcSWarning |> k.SLog
    let err  msg = fun (k: IKernelPub) -> msg |> TrcSError   |> k.SLog

    let dbgf  fmt = sprintf fmt >> dbg
    let infof fmt = sprintf fmt >> info
    let warnf fmt = sprintf fmt >> warn
    let errf  fmt = sprintf fmt >> err 

//
// Utility functions
//

[<AutoOpen>]
module StateUtils =

    let withMessage (msg: string) (stt, envs: Envelope list) = stt, envs, msg
    let withNoEnvelopes stt : _ * Envelope list = stt, []
    let withAsyncFlow (f: _ -> Async<unit>) (stt, envs: Envelope list, msg: string) = stt, envs, msg, f

    let notSupported op stt = sprintf "%s not supported in current state (%O)" op stt

    let aempty = fun _ -> async.Return ()