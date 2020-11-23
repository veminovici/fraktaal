namespace Testly.DSystems.Kernel

module Ids = 

    open System
    open Xunit

    open Simplee.DSystems

    [<Fact>]
    let ``Process Id`` () =

        let myId = "myid"

        let f =
            myId
            |> ProcessId.ofStr
            |> sprintf "%O"
            |> (=) myId

        Assert.True f

    [<Fact>]
    let ``Empty Process Id`` () =

        let f =
            ProcessId.empty
            |> sprintf "%O"
            |> (=) ""

        Assert.True f

    [<Fact>]
    let ``Pid of Process Id`` () =

        let myId = "myid"

        let f =
            myId
            |> ProcessId.ofStr
            |> ProcessId.pid
            |> (=) myId

        Assert.True f

    [<Fact>]
    let ``Process Id and From Id`` () =

        let myId = "myId"

        let f =
            myId
            |> ProcessId.ofStr
            |> ProcessId.toFromId
            |> ProcessId.ofFromId
            |> ProcessId.pid
            |> (=) myId

        Assert.True f

    [<Fact>]
    let ``Process Id and To Id`` () =

        let myId = "myId"

        let f =
            myId
            |> ProcessId.ofStr
            |> ProcessId.toToId
            |> ProcessId.ofToId
            |> ProcessId.pid
            |> (=) myId

        Assert.True f

    [<Fact>]
    let ``From Id`` () =

        let myId = "myid"

        let f =
            myId
            |> FromId.ofStr
            |> sprintf "%O"
            |> (=) myId

        Assert.True f

    [<Fact>]
    let ``Empty From Id`` () =

        let f =
            FromId.empty
            |> sprintf "%O"
            |> (=) ""

        Assert.True f

    [<Fact>]
    let ``Fid of From Id`` () =

        let myId = "myid"

        let f =
            myId
            |> FromId.ofStr
            |> FromId.fid
            |> (=) myId

        Assert.True f

    [<Fact>]
    let ``From Id and Process Id`` () =

        let myId = "myId"

        let f =
            myId
            |> FromId.ofStr
            |> FromId.toProcessId
            |> FromId.ofProcessId
            |> FromId.fid
            |> (=) myId

        Assert.True f

    [<Fact>]
    let ``From Id and To Id`` () =

        let myId = "myId"

        let f =
            myId
            |> FromId.ofStr
            |> FromId.toToId
            |> FromId.ofToId
            |> FromId.fid
            |> (=) myId

        Assert.True f

    [<Fact>]
    let ``To Id`` () =

        let myId = "myid"

        let f =
            myId
            |> ToId.ofStr
            |> sprintf "%O"
            |> (=) myId

        Assert.True f

    [<Fact>]
    let ``Empty To Id`` () =

        let f =
            ToId.empty
            |> sprintf "%O"
            |> (=) ""

        Assert.True f

    [<Fact>]
    let ``Tid of To Id`` () =

        let myId = "myid"

        let f =
            myId
            |> ToId.ofStr
            |> ToId.tid
            |> (=) myId

        Assert.True f

    [<Fact>]
    let ``To Id and Process Id`` () =

        let myId = "myId"

        let f =
            myId
            |> ToId.ofStr
            |> ToId.toProcessId
            |> ToId.ofProcessId
            |> ToId.tid
            |> (=) myId

        Assert.True f

    [<Fact>]
    let ``To Id and From Id`` () =

        let myId = "myId"

        let f =
            myId
            |> ToId.ofStr
            |> ToId.toFromId
            |> ToId.ofFromId
            |> ToId.tid
            |> (=) myId

        Assert.True f

    [<Fact>]
    let ``Session Id`` () =

        let myId = "myid"

        let f =
            myId
            |> SessionId.ofStr
            |> sprintf "%O"
            |> (=) myId

        Assert.True f

    [<Fact>]
    let ``Empty Session Id`` () =

        let f =
            SessionId.empty
            |> sprintf "%O"
            |> (=) ""

        Assert.True f

    [<Fact>]
    let ``Sid of SessionId`` () =

        let myId = "myid"

        let f =
            myId
            |> SessionId.ofStr
            |> SessionId.sid
            |> (=) myId

        Assert.True f

    [<Fact>]
    let ``Envelope Id`` () =

        let myId = "myid"

        let f =
            myId
            |> EnvelopeId.ofStr
            |> sprintf "%O"
            |> (=) myId

        Assert.True f

    [<Fact>]
    let ``Empty EnvelopeId`` () =

        let f =
            EnvelopeId.empty
            |> sprintf "%O"
            |> (=) ""

        Assert.True f

    [<Fact>]
    let ``Eid of EnvelopeId`` () =

        let myId = "myid"

        let f =
            myId
            |> EnvelopeId.ofStr
            |> EnvelopeId.eid
            |> (=) myId

        Assert.True f

    [<Fact>]
    let ``Rnd of EnvelopeId`` () =

        let myId = "myid"

        let f =
            ()
            |> EnvelopeId.rnd
            |> EnvelopeId.eid
            |> (<>) ""

        Assert.True f
