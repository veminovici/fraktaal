namespace Simplee.DSystems

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

    let internal kid = ofStr "_krnl_fid_"

    let toProcessId (FromId fid)    = ProcessId fid
    let ofProcessId (ProcessId pid) = FromId pid

    let toToId      (FromId fid)    = ToId fid
    let ofToId      (ToId tid)      = FromId tid

[<RequireQualifiedAccess>]
module ToId =
    let ofStr = ToId
    let empty = ofStr ""

    let internal kid = ofStr "_krnl_tid_"

    let toProcessId (ToId tid)      = ProcessId tid
    let ofProcessId (ProcessId pid) = ToId pid

    let toFromId    (ToId tid)      = FromId tid
    let ofFromId    (FromId fid)    = ToId fid

[<RequireQualifiedAccess>]
module SessionId =
    let ofStr = SessionId
    let empty = ofStr ""

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

